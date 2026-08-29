module Data.Sectile.System.Linux
  ( -- * System segments
    uptime,
    memory,
    load,
    cpu,
    disk,
    networkUp,
    networkDown,
    battery,
    thermal,
    wifi,
  )
where

import qualified Control.Exception as Exception
import Control.Monad.State (State)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (mapMaybe)
import qualified Data.Sectile.Tmux as Colour
import Data.Sectile.Types
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Read as T
import qualified Data.Time.Clock.POSIX as POSIX
import Numeric (showFFloat)
import qualified System.Directory as Dir
import qualified System.Exit as Exit
import qualified System.Process as Process

-- | Display system uptime by reading @\/proc\/uptime@.
--
-- Formats the uptime as @Xd Xh Xm@.
--
-- Example:
--
-- > import Data.Sectile
-- >
-- > uptimeSegment :: Segment IO
-- > uptimeSegment = uptime "uptime"
-- > -- Renders e.g. "3d 2h 15m"
uptime :: Name -> Segment IO
uptime name@(Name nameB) =
  Segment $ do
    result <- tryReadFile "/proc/uptime"
    let (txt, bnds) = case result of
          Right content ->
            case parseUptime name content of
              Just (formatted, b) -> (formatted, b)
              Nothing -> (errMsg nameB, HashMap.empty)
          Left _ -> (errMsg nameB, HashMap.empty)
    pure $ do
      _ <- appendBindings bnds
      mkFormatted nameB "uptime" txt []

-- | Display memory usage by reading @\/proc\/meminfo@.
--
-- Shows used and total memory in human-readable format, e.g. @"8.2GiB / 15.6GiB (52%)"@.
--
-- Example:
--
-- > import Data.Sectile
-- >
-- > memSegment :: Segment IO
-- > memSegment = memory "mem"
-- > -- Renders e.g. "8.2GiB / 15.6GiB (52%)"
memory :: Name -> Segment IO
memory name@(Name nameB) =
  Segment $ do
    result <- tryReadFile "/proc/meminfo"
    let (txt, bnds) = case result of
          Right content ->
            case parseMeminfo name content of
              Just (formatted, b) -> (formatted, b)
              Nothing -> (errMsg nameB, HashMap.empty)
          Left _ -> (errMsg nameB, HashMap.empty)
    pure $ do
      _ <- appendBindings bnds
      mkFormatted nameB "memory" txt []

-- | Display system load averages by reading @\/proc\/loadavg@.
--
-- Shows the 1, 5, and 15 minute load averages.
--
-- Example:
--
-- > import Data.Sectile
-- >
-- > loadSegment :: Segment IO
-- > loadSegment = load "load"
-- > -- Renders e.g. "1.59 1.29 1.39"
load :: Name -> Segment IO
load name@(Name nameB) =
  Segment $ do
    result <- tryReadFile "/proc/loadavg"
    let (txt, bnds) = case result of
          Right content ->
            case parseLoadavg name content of
              Just (formatted, b) -> (formatted, b)
              Nothing -> (errMsg nameB, HashMap.empty)
          Left _ -> (errMsg nameB, HashMap.empty)
    pure $ do
      _ <- appendBindings bnds
      mkFormatted nameB "load" txt []

-- | Display CPU usage percentage by reading @\/proc\/stat@.
--
-- Shows the aggregate CPU usage as a percentage.
--
-- Example:
--
-- > import Data.Sectile
-- >
-- > cpuSegment :: Segment IO
-- > cpuSegment = cpu "cpu"
-- > -- Renders e.g. "5.6%"
cpu :: Name -> Segment IO
cpu name@(Name nameB) =
  Segment $ do
    result <- tryReadFile "/proc/stat"
    let (txt, bnds) = case result of
          Right content ->
            case parseCpuUsage name content of
              Just (formatted, b) -> (formatted, b)
              Nothing -> (errMsg nameB, HashMap.empty)
          Left _ -> (errMsg nameB, HashMap.empty)
    pure $ do
      _ <- appendBindings bnds
      mkFormatted nameB "cpu" txt []

-- | Display disk usage for a given mount point.
--
-- Uses 'System.Process' to call @df@.
--
-- Shows used, total, and percentage, e.g. @"\/" 450GiB (88%)@.
--
-- Example:
--
-- > import Data.Sectile
-- >
-- > diskSegment :: Segment IO
-- > diskSegment = disk "disk" "/"
-- > -- Renders e.g. "\/  1.5TiB / 1.8TiB (88%)"
disk :: Name -> FilePath -> Segment IO
disk name@(Name nameB) mountPoint =
  Segment $ do
    (exitCode, out, _) <- Process.readProcessWithExitCode "df" ["--output=size,used,avail,pcent", "-B1024", mountPoint] ""
    let (txt, bnds) = case exitCode of
          Exit.ExitSuccess ->
            case parseDiskUsage name (T.pack out) of
              Just (formatted, b) -> (formatted, b)
              Nothing -> (errMsg nameB, HashMap.empty)
          _ -> (errMsg nameB, HashMap.empty)
    pure $ do
      _ <- appendBindings bnds
      mkFormatted nameB "disk" txt [("MountPoint", T.pack mountPoint)]

-- | Display network speed for a given list of interfaces.
networkStats :: Name -> [T.Text] -> NetDirection -> Segment IO
networkStats name@(Name nameB) ifaces direction = Segment $ do
  contentRes <- tryReadFile "/proc/net/dev"
  case contentRes of
    Left _ -> pure $ mkFormatted nameB typeName (errMsg nameB) []
    Right content -> do
      let statsList = mapMaybe (\iface -> parseNetDevBytes iface direction content) ifaces
      case statsList of
        [] -> pure $ mkFormatted nameB typeName (errMsg nameB) []
        _ -> do
          let totalBytes = sum statsList
          now <- POSIX.getPOSIXTime
          let nowMs = round (now * 1000) :: Int

          sessionOutRes <-
            (Right <$> Process.readProcessWithExitCode "tmux" ["display-message", "-p", "#S"] "")
              `Exception.catch` (\(_ :: IOError) -> pure $ Left ())
          let session = case sessionOutRes of
                Right (Exit.ExitSuccess, out, _) -> T.unpack (T.strip (T.pack out))
                _ -> "default"

          let segName = T.unpack $ TL.toStrict $ TLE.decodeUtf8 $ B.toLazyByteString nameB
          let memFile = "/tmp/tmux-net-speeds-mem-" <> session <> "-" <> segName

          fileExists <- Dir.doesFileExist memFile
          rate <-
            if fileExists
              then do
                fileContent <- tryReadFile memFile
                case fileContent of
                  Right fc -> do
                    case T.words (T.strip fc) of
                      [tsStr, bytesStr] -> do
                        case (readInt tsStr, readInt bytesStr) of
                          (Just tsPrev, Just bytesPrev) -> do
                            let dt = nowMs - tsPrev
                            if dt > 0
                              then pure $ Just $ (totalBytes - bytesPrev) * 1000 `div` dt
                              else pure Nothing
                          _ -> pure Nothing
                      _ -> pure Nothing
                  Left _ -> pure Nothing
              else pure Nothing

          _ <- tryWriteFile memFile (T.pack (show nowMs) <> " " <> T.pack (show totalBytes))

          let (txt, bnds) = case rate of
                Just r -> (formatKiB (r `div` 1024) <> "/s", unitBindings "B/s" name (fromIntegral r))
                Nothing -> ("  -  B/s", unitBindings "B/s" name 0)

          pure $ do
            _ <- appendBindings bnds
            mkFormatted nameB typeName txt [("Interfaces", T.intercalate "," ifaces)]
  where
    typeName = case direction of
      NetTransmit -> "networkUp"
      NetReceive -> "networkDown"

-- | Display network upload speed for a given list of interfaces.
networkUp :: Name -> [T.Text] -> Segment IO
networkUp name ifaces = networkStats name ifaces NetTransmit

-- | Display network download speed for a given list of interfaces.
networkDown :: Name -> [T.Text] -> Segment IO
networkDown name ifaces = networkStats name ifaces NetReceive

-- | Display battery capacity and status by reading @\/sys\/class\/power_supply\/BAT*@.
battery :: Name -> String -> Segment IO
battery name@(Name nameB) bat =
  Segment $ do
    capRes <- tryReadFile ("/sys/class/power_supply/" <> bat <> "/capacity")
    statRes <- tryReadFile ("/sys/class/power_supply/" <> bat <> "/status")
    let (txt, bnds) = case (capRes, statRes) of
          (Right cap, Right stat) ->
            let capT = T.strip cap
                statT = T.strip stat
                prefix = case statT of
                  "Charging" -> "CHG"
                  "Discharging" -> "BAT"
                  "Full" -> "FULL"
                  _ -> "UNK"
                val = case readDouble capT of
                  Just v -> v / 100
                  Nothing -> 0
             in (prefix <> " " <> capT <> "%", percentBindings name val)
          _ -> (errMsg nameB, HashMap.empty)
    pure $ do
      _ <- appendBindings bnds
      mkFormatted nameB "battery" txt [("Battery", T.pack bat)]

-- | Display system temperature by reading @\/sys\/class\/thermal\/thermal_zone*\/temp@.
thermal :: Name -> String -> Segment IO
thermal name@(Name nameB) zone =
  Segment $ do
    res <- tryReadFile ("/sys/class/thermal/" <> zone <> "/temp")
    let (txt, bnds) = case res of
          Right tempStr -> case readDouble (T.strip tempStr) of
            Just temp -> (T.pack (show (round (temp / 1000) :: Int)) <> "C", unitBindings "C" name (temp / 1000))
            Nothing -> (errMsg nameB, HashMap.empty)
          Left _ -> (errMsg nameB, HashMap.empty)
    pure $ do
      _ <- appendBindings bnds
      mkFormatted nameB "thermal" txt [("Zone", T.pack zone)]

-- | Display WiFi link quality by reading @\/proc\/net\/wireless@.
wifi :: Name -> String -> Segment IO
wifi (Name nameB) iface =
  Segment $ do
    res <- tryReadFile "/proc/net/wireless"
    let txt = case res of
          Right content -> case parseWifi iface content of
            Just formatted -> formatted
            Nothing -> errMsg nameB
          Left _ -> errMsg nameB
    pure $ mkFormatted nameB "wifi" txt [("Interface", T.pack iface)]

-- Internal helpers

data NetDirection = NetReceive | NetTransmit

-- | Build an error message from a segment name.
errMsg :: B.Builder -> T.Text
errMsg name = "Error on " <> TL.toStrict (TLE.decodeUtf8 (B.toLazyByteString name))

-- | Build a 'Formatted' value with standard explain structure.
mkFormatted :: B.Builder -> T.Text -> T.Text -> [(T.Text, T.Text)] -> State Env Formatted
mkFormatted name typeName txt extraFields = do
  currentSt <- currentStyle
  bnds <- currentBindings
  let (finalStyle, rendered) = Colour.parseAnsiChunks currentSt txt
      explain f =
        DetailList $
          [ DetailPlain $ "Name: " <> name,
            DetailPlain $ "Type: " <> T.encodeUtf8Builder typeName,
            DetailPlain $ "Value: " <> T.encodeUtf8Builder txt,
            DetailPlain $ "Rendered: " <> f rendered
          ]
            <> map (\(k, v) -> DetailPlain $ T.encodeUtf8Builder k <> ": " <> T.encodeUtf8Builder v) extraFields
            <> (if HashMap.null bnds then [] else [DetailPlain "Bindings:", DetailNested $ DetailList [DetailPlain (T.encodeUtf8Builder k <> " = " <> B.lazyByteString (Aeson.encode v)) | (k, v) <- HashMap.toList bnds]])
  _ <- updateStyle (const finalStyle)
  pure Formatted {..}

-- | Try to read a file, catching any IOException.
tryReadFile :: FilePath -> IO (Either IOError T.Text)
tryReadFile path =
  (Right . T.pack <$> readFile path)
    `Exception.catch` (\(e :: IOError) -> pure $ Left e)

-- | Parse /proc/uptime: "12345.67 89012.34" -> "Xd Xh Xm"
parseUptime :: Name -> T.Text -> Maybe (T.Text, HashMap.HashMap T.Text Aeson.Value)
parseUptime name content = case T.double (T.strip content) of
  Right (seconds :: Double, _) ->
    let totalMinutes = floor seconds `div` 60 :: Int
        minutes = totalMinutes `mod` 60
        hours = (totalMinutes `div` 60) `mod` 24
        days = totalMinutes `div` (60 * 24)
        txt = T.pack (show days) <> "d " <> T.pack (show hours) <> "h " <> T.pack (show minutes) <> "m"
        bnds = unitBindings "s" name seconds
     in Just (txt, bnds)
  Left _ -> Nothing

-- | Parse /proc/meminfo to extract MemTotal, MemAvailable.
parseMeminfo :: Name -> T.Text -> Maybe (T.Text, HashMap.HashMap T.Text Aeson.Value)
parseMeminfo (Name nameB) content =
  let lns = T.lines content
      findField key = case filter (T.isPrefixOf key) lns of
        (l : _) -> case T.decimal (T.strip $ T.drop 1 $ T.dropWhile (/= ':') l) of
          Right (kb :: Int, _) -> Just kb
          Left _ -> Nothing
        [] -> Nothing
   in case (findField "MemTotal:", findField "MemAvailable:") of
        (Just totalKB, Just availKB) ->
          let total = fromIntegral totalKB * 1024 :: Double
              avail = fromIntegral availKB * 1024 :: Double
              used = total - avail
              pct = used / total
              bnds =
                unitBindings "B" (Name (nameB <> ".total")) total
                  <> unitBindings "B" (Name (nameB <> ".used.total")) used
                  <> percentBindings (Name (nameB <> ".used")) pct
                  <> unitBindings "B" (Name (nameB <> ".free.total")) avail
                  <> percentBindings (Name (nameB <> ".free")) (avail / total)
              txt =
                formatKiB (round (used / 1024))
                  <> " / "
                  <> formatKiB (round (total / 1024))
                  <> " ("
                  <> T.pack (show (round (pct * 100) :: Int))
                  <> "%)"
           in Just (txt, bnds)
        _ -> Nothing

-- | Parse /proc/loadavg: "1.59 1.29 1.39 3/4059 665034" -> "1.59 1.29 1.39"
parseLoadavg :: Name -> T.Text -> Maybe (T.Text, HashMap.HashMap T.Text Aeson.Value)
parseLoadavg (Name nameB) content =
  let ws = T.words (T.strip content)
   in case ws of
        (l1 : l5 : l15 : _) ->
          case (readDouble l1, readDouble l5, readDouble l15) of
            (Just n1, Just n5, Just n15) ->
              let txt = l1 <> " " <> l5 <> " " <> l15
                  nameT = T.decodeUtf8 (LBS.toStrict (B.toLazyByteString nameB))
                  bnds =
                    HashMap.fromList
                      [ (nameT <> ".1m.raw", Aeson.Number (realToFrac n1)),
                        (nameT <> ".5m.raw", Aeson.Number (realToFrac n5)),
                        (nameT <> ".15m.raw", Aeson.Number (realToFrac n15))
                      ]
               in Just (txt, bnds)
            _ -> Nothing
        _ -> Nothing

-- | Parse /proc/stat cpu line to get usage percentage.
parseCpuUsage :: Name -> T.Text -> Maybe (T.Text, HashMap.HashMap T.Text Aeson.Value)
parseCpuUsage name content =
  let lns = T.lines content
   in case filter (T.isPrefixOf "cpu ") lns of
        (cpuLine : _) ->
          let ws = drop 1 $ T.words cpuLine
              nums = mapMaybe readDouble ws
           in case nums of
                (user : nice : system : idle : iowait : irq : softirq : steal : _) ->
                  let total = user + nice + system + idle + iowait + irq + softirq + steal
                      busy = total - idle - iowait
                      pct = busy / total
                      bnds = percentBindings name pct
                   in Just (T.pack (showFFloat1 (pct * 100)) <> "%", bnds)
                _ -> Nothing
        _ -> Nothing

-- | Parse disk usage from df --output=size,used,avail,pcent -B1024 output.
parseDiskUsage :: Name -> T.Text -> Maybe (T.Text, HashMap.HashMap T.Text Aeson.Value)
parseDiskUsage (Name nameB) content =
  let lns = T.lines content
   in case lns of
        (_ : dataLine : _) ->
          case T.words dataLine of
            (sizeT : usedT : availT : pcentT : _) ->
              case (readDouble sizeT, readDouble usedT, readDouble availT) of
                (Just sizeKB, Just usedKB, Just availKB) ->
                  let size = sizeKB * 1024
                      used = usedKB * 1024
                      avail = availKB * 1024
                      bnds =
                        unitBindings "B" (Name (nameB <> ".total")) size
                          <> unitBindings "B" (Name (nameB <> ".used.total")) used
                          <> percentBindings (Name (nameB <> ".used")) (used / size)
                          <> unitBindings "B" (Name (nameB <> ".free.total")) avail
                          <> percentBindings (Name (nameB <> ".free")) (avail / size)
                      txt = formatKiB (round sizeKB) <> " (" <> pcentT <> ")"
                   in Just (txt, bnds)
                _ -> Nothing
            _ -> Nothing
        _ -> Nothing

-- | Parse /proc/net/dev for a specific interface returning bytes.
parseNetDevBytes :: T.Text -> NetDirection -> T.Text -> Maybe Int
parseNetDevBytes iface direction content =
  let lns = T.lines content
      ifacePrefix = T.strip iface <> ":"
      matchLine l =
        let stripped = T.stripStart l
         in T.isPrefixOf ifacePrefix stripped
   in case filter matchLine lns of
        (l : _) ->
          let parts = T.words $ T.drop (T.length ifacePrefix) $ T.stripStart l
              -- Receive: bytes(0) packets(1) ...
              -- Transmit: bytes(8) packets(9) ...
              idx = case direction of
                NetReceive -> 0
                NetTransmit -> 8
           in case drop idx parts of
                (val : _) -> readInt val
                _ -> Nothing
        _ -> Nothing

-- | Try to write a file, catching any IOException.
tryWriteFile :: FilePath -> T.Text -> IO (Either IOError ())
tryWriteFile path content =
  (Right <$> writeFile path (T.unpack content))
    `Exception.catch` (\(e :: IOError) -> pure $ Left e)

-- | Format kibibytes to human-readable.
formatKiB :: Int -> T.Text
formatKiB kb
  | kb >= 1024 ^ (4 :: Int) = T.pack (showFFloat1 (fromIntegral kb / (1024.0 ** 4) :: Double)) <> "EiB"
  | kb >= 1024 ^ (3 :: Int) = T.pack (showFFloat1 (fromIntegral kb / (1024.0 ** 3) :: Double)) <> "TiB"
  | kb >= 1024 ^ (2 :: Int) = T.pack (showFFloat1 (fromIntegral kb / (1024.0 ** 2) :: Double)) <> "GiB"
  | kb >= 1024 = T.pack (showFFloat1 (fromIntegral kb / 1024.0 :: Double)) <> "MiB"
  | otherwise = T.pack (show kb) <> "KiB"

-- | Show a Double with 1 decimal place.
showFFloat1 :: Double -> String
showFFloat1 x = showFFloat (Just n) x ""
  where
    n
      | abs x >= 100 = 0
      | abs x >= 10 = 1
      | otherwise = 2

-- | Read an Int from Text, returning Nothing on failure.
readInt :: T.Text -> Maybe Int
readInt t = case T.decimal t of
  Right (n, _) -> Just n
  Left _ -> Nothing

-- | Read a Double from Text, returning Nothing on failure.
readDouble :: T.Text -> Maybe Double
readDouble t = case T.double t of
  Right (n, _) -> Just n
  Left _ -> Nothing

-- | Parse /proc/net/wireless for a specific interface.
parseWifi :: String -> T.Text -> Maybe T.Text
parseWifi iface content =
  let lns = T.lines content
      ifacePrefix = T.pack iface <> ":"
      matchLine l = T.isPrefixOf ifacePrefix (T.stripStart l)
   in case filter matchLine lns of
        (l : _) ->
          let parts = T.words $ T.drop (T.length ifacePrefix) $ T.stripStart l
           in case parts of
                (_status : link : level : _) -> Just $ T.strip (T.dropWhileEnd (== '.') link) <> "% " <> T.strip (T.dropWhileEnd (== '.') level) <> "dBm"
                _ -> Nothing
        _ -> Nothing

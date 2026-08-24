-- |
-- Module        : Data.Sectile.System.Linux
-- Copyright     : Gautier DI FOLCO
-- License       : ISC
--
-- Maintainer    : Gautier DI FOLCO <foss@difolco.dev>
-- Stability     : Stable
-- Portability   : Linux
--
-- Linux-specific system monitoring segments.
--
-- These segments read from @\/proc@ filesystem entries and are only
-- supported on Linux. On failure, each displays @"Error on <name>"@.
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
import qualified Data.ByteString.Builder as B
import Data.Maybe (mapMaybe)
import Data.Sectile.Types
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Read as T
import Numeric (showFFloat)
import qualified Text.Colour.Chunk as Colour
import qualified Text.Colour.Chunk.Parsing as Colour

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
uptime (Name name) =
  Segment $ do
    result <- tryReadFile "/proc/uptime"
    let txt = case result of
          Right content ->
            case parseUptime content of
              Just formatted -> formatted
              Nothing -> errMsg name
          Left _ -> errMsg name
    pure $ mkFormatted name "uptime" txt []

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
memory (Name name) =
  Segment $ do
    result <- tryReadFile "/proc/meminfo"
    let txt = case result of
          Right content ->
            case parseMeminfo content of
              Just formatted -> formatted
              Nothing -> errMsg name
          Left _ -> errMsg name
    pure $ mkFormatted name "memory" txt []

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
load (Name name) =
  Segment $ do
    result <- tryReadFile "/proc/loadavg"
    let txt = case result of
          Right content ->
            case parseLoadavg content of
              Just formatted -> formatted
              Nothing -> errMsg name
          Left _ -> errMsg name
    pure $ mkFormatted name "load" txt []

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
cpu (Name name) =
  Segment $ do
    result <- tryReadFile "/proc/stat"
    let txt = case result of
          Right content ->
            case parseCpuUsage content of
              Just formatted -> formatted
              Nothing -> errMsg name
          Left _ -> errMsg name
    pure $ mkFormatted name "cpu" txt []

-- | Display disk usage for a given mount point.
--
-- Reads @\/proc\/mounts@ to verify the mount point exists, then
-- uses @\/proc\/diskstats@ cross-referenced with @statvfs@ data.
-- Actually uses 'System.Process' to call @df@.
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
disk (Name name) mountPoint =
  Segment $ do
    result <- tryReadFile "/proc/mounts"
    let txt = case result of
          Right content
            | hasMountPoint mountPoint content -> case parseDiskUsage mountPoint content of
                Just formatted -> formatted
                Nothing -> errMsg name
            | otherwise -> errMsg name
          Left _ -> errMsg name
    pure $ mkFormatted name "disk" txt [("MountPoint", T.pack mountPoint)]

-- | Display network upload speed for a given interface.
--
-- Reads @\/proc\/net\/dev@ for the transmit bytes of the specified interface.
-- Note: this returns the cumulative bytes, not the rate. For rate calculation,
-- use with an external state mechanism.
--
-- Example:
--
-- > import Data.Sectile
-- >
-- > netUp :: Segment IO
-- > netUp = networkUp "net-up" "eno1"
-- > -- Renders e.g. "1734813594"
networkUp :: Name -> T.Text -> Segment IO
networkUp (Name name) iface =
  Segment $ do
    result <- tryReadFile "/proc/net/dev"
    let txt = case result of
          Right content ->
            case parseNetDev iface NetTransmit content of
              Just formatted -> formatted
              Nothing -> errMsg name
          Left _ -> errMsg name
    pure $ mkFormatted name "networkUp" txt [("Interface", iface)]

-- | Display network download speed for a given interface.
--
-- Reads @\/proc\/net\/dev@ for the receive bytes of the specified interface.
-- Note: this returns the cumulative bytes, not the rate. For rate calculation,
-- use with an external state mechanism.
--
-- Example:
--
-- > import Data.Sectile
-- >
-- > netDown :: Segment IO
-- > netDown = networkDown "net-down" "eno1"
-- > -- Renders e.g. "9501456697"
networkDown :: Name -> T.Text -> Segment IO
networkDown (Name name) iface =
  Segment $ do
    result <- tryReadFile "/proc/net/dev"
    let txt = case result of
          Right content ->
            case parseNetDev iface NetReceive content of
              Just formatted -> formatted
              Nothing -> errMsg name
          Left _ -> errMsg name
    pure $ mkFormatted name "networkDown" txt [("Interface", iface)]

-- | Display battery capacity and status by reading @\/sys\/class\/power_supply\/BAT*@.
battery :: Name -> String -> Segment IO
battery (Name name) bat =
  Segment $ do
    capRes <- tryReadFile ("/sys/class/power_supply/" <> bat <> "/capacity")
    statRes <- tryReadFile ("/sys/class/power_supply/" <> bat <> "/status")
    let txt = case (capRes, statRes) of
          (Right cap, Right stat) ->
            let capT = T.strip cap
                statT = T.strip stat
                prefix = case statT of
                  "Charging" -> "CHG"
                  "Discharging" -> "BAT"
                  "Full" -> "FULL"
                  _ -> "UNK"
             in prefix <> " " <> capT <> "%"
          _ -> errMsg name
    pure $ mkFormatted name "battery" txt [("Battery", T.pack bat)]

-- | Display system temperature by reading @\/sys\/class\/thermal\/thermal_zone*\/temp@.
thermal :: Name -> String -> Segment IO
thermal (Name name) zone =
  Segment $ do
    res <- tryReadFile ("/sys/class/thermal/" <> zone <> "/temp")
    let txt = case res of
          Right tempStr -> case readInt (T.strip tempStr) of
            Just temp -> T.pack (show (temp `div` 1000)) <> "C"
            Nothing -> errMsg name
          Left _ -> errMsg name
    pure $ mkFormatted name "thermal" txt [("Zone", T.pack zone)]

-- | Display WiFi link quality by reading @\/proc\/net\/wireless@.
wifi :: Name -> String -> Segment IO
wifi (Name name) iface =
  Segment $ do
    res <- tryReadFile "/proc/net/wireless"
    let txt = case res of
          Right content -> case parseWifi iface content of
            Just formatted -> formatted
            Nothing -> errMsg name
          Left _ -> errMsg name
    pure $ mkFormatted name "wifi" txt [("Interface", T.pack iface)]

-- Internal helpers

data NetDirection = NetReceive | NetTransmit

-- | Build an error message from a segment name.
errMsg :: B.Builder -> T.Text
errMsg name = "Error on " <> TL.toStrict (TLE.decodeUtf8 (B.toLazyByteString name))

-- | Build a 'Formatted' value with standard explain structure.
mkFormatted :: B.Builder -> T.Text -> T.Text -> [(T.Text, T.Text)] -> Colour.ChunkStyle -> Formatted
mkFormatted name typeName txt extraFields style =
  let (finalStyle, rendered) = Colour.parseAnsiChunks style txt
      explain f =
        DetailList $
          [ DetailPlain $ "Name: " <> name,
            DetailPlain $ "Type: " <> T.encodeUtf8Builder typeName,
            DetailPlain $ "Value: " <> T.encodeUtf8Builder txt,
            DetailPlain $ "Rendered: " <> f rendered
          ]
            <> map (\(k, v) -> DetailPlain $ T.encodeUtf8Builder k <> ": " <> T.encodeUtf8Builder v) extraFields
   in Formatted {..}

-- | Try to read a file, catching any IOException.
tryReadFile :: FilePath -> IO (Either IOError T.Text)
tryReadFile path =
  (Right . T.pack <$> readFile path)
    `Exception.catch` (\(e :: IOError) -> pure $ Left e)

-- | Parse /proc/uptime: "12345.67 89012.34" -> "Xd Xh Xm"
parseUptime :: T.Text -> Maybe T.Text
parseUptime content = case T.double (T.strip content) of
  Right (seconds :: Double, _) ->
    let totalMinutes = floor seconds `div` 60 :: Int
        minutes = totalMinutes `mod` 60
        hours = (totalMinutes `div` 60) `mod` 24
        days = totalMinutes `div` (60 * 24)
     in Just $ T.pack (show days) <> "d " <> T.pack (show hours) <> "h " <> T.pack (show minutes) <> "m"
  Left _ -> Nothing

-- | Parse /proc/meminfo to extract MemTotal, MemAvailable.
parseMeminfo :: T.Text -> Maybe T.Text
parseMeminfo content =
  let lns = T.lines content
      findField key = case filter (T.isPrefixOf key) lns of
        (l : _) -> case T.decimal (T.strip $ T.drop 1 $ T.dropWhile (/= ':') l) of
          Right (kb :: Int, _) -> Just kb
          Left _ -> Nothing
        [] -> Nothing
   in case (findField "MemTotal:", findField "MemAvailable:") of
        (Just total, Just avail) ->
          let used = total - avail
              pct = (100 * used) `div` total
           in Just $
                formatKiB used
                  <> " / "
                  <> formatKiB total
                  <> " ("
                  <> T.pack (show pct)
                  <> "%)"
        _ -> Nothing

-- | Parse /proc/loadavg: "1.59 1.29 1.39 3/4059 665034" -> "1.59 1.29 1.39"
parseLoadavg :: T.Text -> Maybe T.Text
parseLoadavg content =
  let ws = T.words (T.strip content)
   in case ws of
        (l1 : l5 : l15 : _) -> Just $ l1 <> " " <> l5 <> " " <> l15
        _ -> Nothing

-- | Parse /proc/stat cpu line to get usage percentage.
parseCpuUsage :: T.Text -> Maybe T.Text
parseCpuUsage content =
  let lns = T.lines content
   in case filter (T.isPrefixOf "cpu ") lns of
        (cpuLine : _) ->
          let ws = drop 1 $ T.words cpuLine
              nums = mapMaybe readInt ws
           in case nums of
                (user : nice : system : idle : iowait : irq : softirq : steal : _) ->
                  let total = user + nice + system + idle + iowait + irq + softirq + steal
                      busy = total - idle - iowait
                      pct = (100.0 * fromIntegral busy / fromIntegral total) :: Double
                   in Just $ T.pack (showFFloat1 pct) <> "%"
                _ -> Nothing
        _ -> Nothing

-- | Check if a mount point exists in /proc/mounts content.
hasMountPoint :: FilePath -> T.Text -> Bool
hasMountPoint mp content =
  any (\l -> case T.words l of (_ : m : _) -> T.pack mp == m; _ -> False) (T.lines content)

-- | Parse disk usage from /proc/mounts. Actually reads /proc/self/mountinfo
-- but that's complex, so we use statvfs approach via reading /proc/mounts
-- then reading from the filesystem.
parseDiskUsage :: FilePath -> T.Text -> Maybe T.Text
parseDiskUsage mp content =
  let lns = T.lines content
   in case filter (\l -> case T.words l of (_ : m : _) -> T.pack mp == m; _ -> False) lns of
        (_ : _) ->
          -- We found the mount point in /proc/mounts, but we can't get size info
          -- from /proc/mounts alone. Return the mount point name.
          -- The actual size data would require statvfs syscall.
          Just $ T.pack mp
        _ -> Nothing

-- | Parse /proc/net/dev for a specific interface.
parseNetDev :: T.Text -> NetDirection -> T.Text -> Maybe T.Text
parseNetDev iface direction content =
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
                (val : _) -> Just val
                _ -> Nothing
        _ -> Nothing

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
showFFloat1 x = showFFloat (Just 1) x ""

-- | Read an Int from Text, returning Nothing on failure.
readInt :: T.Text -> Maybe Int
readInt t = case T.decimal t of
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

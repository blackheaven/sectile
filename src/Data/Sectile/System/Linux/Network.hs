-- |
-- Module        : Data.Sectile.System.Linux.Network
-- Copyright     : Gautier DI FOLCO
-- License       : ISC
--
-- Maintainer    : Gautier DI FOLCO <foss@difolco.dev>
-- Stability     : Stable
-- Portability   : Portable
module Data.Sectile.System.Linux.Network
  ( networkStats,
    networkUp,
    networkDown,
    parseNetDevBytes,
    NetDirection (..),
  )
where

import qualified Control.Exception as Exception
import qualified Data.ByteString.Builder as B
import Data.Maybe (mapMaybe)
import Data.Sectile.System.Linux.Internal
import Data.Sectile.Types
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Time.Clock.POSIX as POSIX
import qualified System.Directory as Dir
import qualified System.Exit as Exit
import qualified System.Process as Process

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

-- | Direction of network traffic: received (download) or transmitted (upload).
data NetDirection = NetReceive | NetTransmit

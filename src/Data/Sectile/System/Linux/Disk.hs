module Data.Sectile.System.Linux.Disk (disk, parseDiskUsage) where

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
import Data.Sectile.System.Linux.Internal

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

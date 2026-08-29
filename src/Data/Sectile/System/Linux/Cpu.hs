module Data.Sectile.System.Linux.Cpu (cpu, parseCpuUsage) where

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

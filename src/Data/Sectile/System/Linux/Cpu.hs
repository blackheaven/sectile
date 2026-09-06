-- |
-- Module        : Data.Sectile.System.Linux.Cpu
-- Copyright     : Gautier DI FOLCO
-- License       : ISC
--
-- Maintainer    : Gautier DI FOLCO <foss@difolco.dev>
-- Stability     : Stable
-- Portability   : Portable
module Data.Sectile.System.Linux.Cpu (cpu) where

import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (mapMaybe)
import Data.Sectile.System.Linux.Internal
import Data.Sectile.Types
import qualified Data.Text as T

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

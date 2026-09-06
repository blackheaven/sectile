-- |
-- Module        : Data.Sectile.System.Linux.Memory
-- Copyright     : Gautier DI FOLCO
-- License       : ISC
--
-- Maintainer    : Gautier DI FOLCO <foss@difolco.dev>
-- Stability     : Stable
-- Portability   : Portable
--
module Data.Sectile.System.Linux.Memory (memory) where

import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import Data.Sectile.System.Linux.Internal
import Data.Sectile.Types
import qualified Data.Text as T
import qualified Data.Text.Read as T

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
                T.pack (show (round (used / (1024 * 1024 * 1024)) :: Int))
                  <> " GiB ("
                  <> T.pack (show (round (pct * 100) :: Int))
                  <> "%)"
           in Just (txt, bnds)
        _ -> Nothing

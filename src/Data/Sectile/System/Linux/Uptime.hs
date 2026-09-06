-- |
-- Module        : Data.Sectile.System.Linux.Uptime
-- Copyright     : Gautier DI FOLCO
-- License       : ISC
--
-- Maintainer    : Gautier DI FOLCO <foss@difolco.dev>
-- Stability     : Stable
-- Portability   : Portable
module Data.Sectile.System.Linux.Uptime (uptime) where

import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import Data.Sectile.System.Linux.Internal
import Data.Sectile.Types
import qualified Data.Text as T
import qualified Data.Text.Read as T

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

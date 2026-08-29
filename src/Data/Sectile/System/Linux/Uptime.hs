module Data.Sectile.System.Linux.Uptime (uptime, parseUptime) where

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

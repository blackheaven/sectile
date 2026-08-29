module Data.Sectile.System.Linux.Load (load, parseLoadavg) where

import qualified Control.Exception as Exception
import Control.Monad.State (State)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (mapMaybe)
import Data.Sectile.System.Linux.Internal
import qualified Data.Sectile.Tmux as Colour
import Data.Sectile.Types
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Read as T
import qualified Data.Time.Clock.POSIX as POSIX
import GHC.Conc (getNumProcessors)
import Numeric (showFFloat)
import qualified System.Directory as Dir
import qualified System.Exit as Exit
import qualified System.Process as Process

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
    threads <- getNumProcessors
    let (txt, bnds) = case result of
          Right content ->
            case parseLoadavg threads name content of
              Just (formatted, b) -> (formatted, b)
              Nothing -> (errMsg nameB, HashMap.empty)
          Left _ -> (errMsg nameB, HashMap.empty)
    pure $ do
      _ <- appendBindings bnds
      mkFormatted nameB "load" txt []

-- | Parse /proc/loadavg: "1.59 1.29 1.39 3/4059 665034" -> "1.59 1.29 1.39"
parseLoadavg :: Int -> Name -> T.Text -> Maybe (T.Text, HashMap.HashMap T.Text Aeson.Value)
parseLoadavg threads (Name nameB) content =
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
                        (nameT <> ".15m.raw", Aeson.Number (realToFrac n15)),
                        (nameT <> ".threads", Aeson.Number (fromIntegral threads))
                      ]
               in Just (txt, bnds)
            _ -> Nothing
        _ -> Nothing

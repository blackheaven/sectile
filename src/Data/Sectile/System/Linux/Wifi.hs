module Data.Sectile.System.Linux.Wifi (wifi, parseWifi) where

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

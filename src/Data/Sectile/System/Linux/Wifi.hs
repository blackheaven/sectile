-- |
-- Module        : Data.Sectile.System.Linux.Wifi
-- Copyright     : Gautier DI FOLCO
-- License       : ISC
--
-- Maintainer    : Gautier DI FOLCO <foss@difolco.dev>
-- Stability     : Stable
-- Portability   : Portable
module Data.Sectile.System.Linux.Wifi (wifi) where

import Data.Sectile.System.Linux.Internal
import Data.Sectile.Types
import qualified Data.Text as T

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

-- |
-- Module        : Data.Sectile.System.Linux.Battery
-- Copyright     : Gautier DI FOLCO
-- License       : ISC
--
-- Maintainer    : Gautier DI FOLCO <foss@difolco.dev>
-- Stability     : Stable
-- Portability   : Portable
module Data.Sectile.System.Linux.Battery (battery) where

import qualified Data.HashMap.Strict as HashMap
import Data.Sectile.System.Linux.Internal
import Data.Sectile.Types
import qualified Data.Text as T

-- | Display battery capacity and status by reading @\/sys\/class\/power_supply\/BAT*@.
battery :: Name -> String -> Segment IO
battery name@(Name nameB) bat =
  Segment $ do
    capRes <- tryReadFile ("/sys/class/power_supply/" <> bat <> "/capacity")
    statRes <- tryReadFile ("/sys/class/power_supply/" <> bat <> "/status")
    let (txt, bnds) = case (capRes, statRes) of
          (Right cap, Right stat) ->
            let capT = T.strip cap
                statT = T.strip stat
                prefix = case statT of
                  "Charging" -> "CHG"
                  "Discharging" -> "BAT"
                  "Full" -> "FULL"
                  _ -> "UNK"
                val = case readDouble capT of
                  Just v -> v / 100
                  Nothing -> 0
             in (prefix <> " " <> capT <> "%", percentBindings name val)
          _ -> (errMsg nameB, HashMap.empty)
    pure $ do
      _ <- appendBindings bnds
      mkFormatted nameB "battery" txt [("Battery", T.pack bat)]

module Data.Sectile.System.Linux.Battery (battery) where

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

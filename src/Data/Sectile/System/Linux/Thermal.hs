module Data.Sectile.System.Linux.Thermal (thermal) where

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

-- | Display system temperature by reading @\/sys\/class\/thermal\/thermal_zone*\/temp@.
thermal :: Name -> String -> Segment IO
thermal name@(Name nameB) zone =
  Segment $ do
    res <- tryReadFile ("/sys/class/thermal/" <> zone <> "/temp")
    let (txt, bnds) = case res of
          Right tempStr -> case readDouble (T.strip tempStr) of
            Just temp -> (T.pack (show (round (temp / 1000) :: Int)) <> "C", unitBindings "C" name (temp / 1000))
            Nothing -> (errMsg nameB, HashMap.empty)
          Left _ -> (errMsg nameB, HashMap.empty)
    pure $ do
      _ <- appendBindings bnds
      mkFormatted nameB "thermal" txt [("Zone", T.pack zone)]

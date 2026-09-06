-- |
-- Module        : Data.Sectile.System.Linux.Thermal
-- Copyright     : Gautier DI FOLCO
-- License       : ISC
--
-- Maintainer    : Gautier DI FOLCO <foss@difolco.dev>
-- Stability     : Stable
-- Portability   : Portable
module Data.Sectile.System.Linux.Thermal (thermal) where

import qualified Data.HashMap.Strict as HashMap
import Data.Sectile.System.Linux.Internal
import Data.Sectile.Types
import qualified Data.Text as T

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

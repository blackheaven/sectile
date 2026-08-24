-- |
-- Module        : Data.Sectile
-- Copyright     : Gautier DI FOLCO
-- License       : ISC
--
-- Maintainer    : Gautier DI FOLCO <foss@difolco.dev>
-- Stability     : Stable
-- Portability   : Portable
--
-- Composable status line builder.
--
-- @sectile@ lets you compose terminal status lines from reusable segments
-- that can be styled, padded, truncated, and combined.
--
-- Build segments with 'string', 'sh', 'time', or the system monitoring
-- functions from "Data.Sectile.System.Linux". Combine them with 'row' and 'between'.
-- Style them with 'changeStyle', 'forceStyle', and the optics from
-- "Data.Sectile.Style". Control display width with the functions from
-- "Data.Sectile.Display".
--
-- Example:
--
-- > import Data.Sectile
-- > import Data.Sectile.System.Linux
-- > import qualified Data.ByteString.Builder as B
-- > import qualified Text.Colour.Capabilities as Colour
-- >
-- > main :: IO ()
-- > main = do
-- >   let status =
-- >         row mapM "status" $
-- >           between (string " [") (string "] ") $
-- >             [ time "clock" "%H:%M",
-- >               string " | ",
-- >               sh "host" "hostname" Nothing
-- >             ]
-- >   output <- renderSegment Colour.With8Colours status
-- >   B.hPutBuilder stdout output
module Data.Sectile (module X) where

import Data.Sectile.Display as X
import Data.Sectile.Runners as X
import Data.Sectile.Segments as X
import Data.Sectile.Style as X
import Data.Sectile.Themes as X
import Data.Sectile.Types as X

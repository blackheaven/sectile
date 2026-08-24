-- |
-- Module        : Data.Sectile.Themes
-- Copyright     : Gautier DI FOLCO
-- License       : ISC
--
-- Maintainer    : Gautier DI FOLCO <foss@difolco.dev>
-- Stability     : Stable
-- Portability   : Portable
--
-- Predefined colour themes inspired by tmux2k.
--
-- Each 'Theme' provides a complete colour palette that can be used to
-- style status line segments. Themes are based on
-- <https://github.com/2KAbhishek/tmux2k tmux2k> colour schemes.
--
-- Example:
--
-- > import Data.Sectile
-- > import Data.Sectile.Themes
-- > import Data.Sectile.Style
-- >
-- > styledSegment :: Segment IO
-- > styledSegment =
-- >   let t = catppuccin
-- >    in forceStyle (themeStyle t.blue t.black) (string "hello")
module Data.Sectile.Themes
  ( -- * Theme type
    Theme (..),

    -- * Predefined themes
    defaultTheme,
    catppuccin,
    gruvbox,
    monokai,
    onedark,

    -- * Theme helpers
    themeStyle,
  )
where

import qualified Data.Sectile.Tmux as Colour
import Data.Word (Word8)

-- | A complete colour palette for theming status line segments.
--
-- Each field holds a 24-bit RGB colour. The palette covers three
-- intensities (light, normal, dark) of eight hues, plus black, gray,
-- and white.
data Theme = Theme
  { black :: Colour.Colour,
    gray :: Colour.Colour,
    white :: Colour.Colour,
    lightBlue :: Colour.Colour,
    blue :: Colour.Colour,
    darkBlue :: Colour.Colour,
    lightGreen :: Colour.Colour,
    green :: Colour.Colour,
    darkGreen :: Colour.Colour,
    lightOrange :: Colour.Colour,
    orange :: Colour.Colour,
    darkOrange :: Colour.Colour,
    lightPink :: Colour.Colour,
    pink :: Colour.Colour,
    darkPink :: Colour.Colour,
    lightPurple :: Colour.Colour,
    purple :: Colour.Colour,
    darkPurple :: Colour.Colour,
    lightRed :: Colour.Colour,
    red :: Colour.Colour,
    darkRed :: Colour.Colour,
    lightYellow :: Colour.Colour,
    yellow :: Colour.Colour,
    darkYellow :: Colour.Colour
  }
  deriving stock (Show, Eq)

-- | Build a 'Colour.ChunkStyle' with the given foreground and background colours.
--
-- Example:
--
-- > let t = catppuccin
-- >  in forceStyle (themeStyle t.blue t.black) segment
themeStyle :: Colour.Colour -> Colour.Colour -> Colour.ChunkStyle -> Colour.ChunkStyle
themeStyle fg bg style =
  style
    { Colour.chunkStyleForeground = Just fg,
      Colour.chunkStyleBackground = Just bg
    }

-- | Shorthand for constructing a 24-bit colour from RGB components.
rgb :: Word8 -> Word8 -> Word8 -> Colour.Colour
rgb = Colour.Colour24Bit

-- | The default tmux2k theme.
defaultTheme :: Theme
defaultTheme =
  Theme
    { black = rgb 0x00 0x00 0x00,
      gray = rgb 0x3f 0x3f 0x4f,
      white = rgb 0xff 0xff 0xff,
      lightBlue = rgb 0x11 0xdd 0xdd,
      blue = rgb 0x16 0x88 0xf0,
      darkBlue = rgb 0x00 0x00 0xcd,
      lightGreen = rgb 0xcc 0xff 0xcc,
      green = rgb 0x3d 0xd5 0x0a,
      darkGreen = rgb 0x00 0x64 0x00,
      lightOrange = rgb 0xff 0xa0 0x7a,
      orange = rgb 0xff 0xa5 0x00,
      darkOrange = rgb 0xff 0x45 0x00,
      lightPink = rgb 0xff 0xb6 0xc1,
      pink = rgb 0xff 0x69 0xb4,
      darkPink = rgb 0xff 0x14 0x93,
      lightPurple = rgb 0xdd 0xa0 0xdd,
      purple = rgb 0xbf 0x58 0xff,
      darkPurple = rgb 0x4b 0x00 0x82,
      lightRed = rgb 0xff 0x4a 0x6a,
      red = rgb 0xff 0x1f 0x1f,
      darkRed = rgb 0x80 0x00 0x00,
      lightYellow = rgb 0xff 0xfa 0xcd,
      yellow = rgb 0xff 0xd2 0x1a,
      darkYellow = rgb 0xb8 0x86 0x0b
    }

-- | The Catppuccin Macchiato theme.
catppuccin :: Theme
catppuccin =
  Theme
    { black = rgb 0x1e 0x20 0x30,
      gray = rgb 0x3f 0x3f 0x3f,
      white = rgb 0xff 0xff 0xff,
      lightBlue = rgb 0x91 0xd7 0xe3,
      blue = rgb 0x8a 0xad 0xf4,
      darkBlue = rgb 0x00 0x00 0x8b,
      lightGreen = rgb 0x8b 0xd5 0xca,
      green = rgb 0xa6 0xda 0x95,
      darkGreen = rgb 0x00 0x64 0x00,
      lightOrange = rgb 0xff 0xa0 0x7a,
      orange = rgb 0xf5 0xa9 0x7f,
      darkOrange = rgb 0xff 0x45 0x00,
      lightPink = rgb 0xff 0xb6 0xc1,
      pink = rgb 0xf5 0xbd 0xe6,
      darkPink = rgb 0xff 0x14 0x93,
      lightPurple = rgb 0xdd 0xa0 0xdd,
      purple = rgb 0xb6 0xa0 0xfe,
      darkPurple = rgb 0x4b 0x00 0x82,
      lightRed = rgb 0xee 0x99 0xa0,
      red = rgb 0xed 0x87 0x96,
      darkRed = rgb 0xb0 0x30 0x60,
      lightYellow = rgb 0xff 0xfa 0xcd,
      yellow = rgb 0xee 0xd4 0x9f,
      darkYellow = rgb 0xb8 0x86 0x0b
    }

-- | The Gruvbox theme.
gruvbox :: Theme
gruvbox =
  Theme
    { black = rgb 0x28 0x28 0x28,
      gray = rgb 0x4f 0x4f 0x4f,
      white = rgb 0xeb 0xdb 0xb2,
      lightBlue = rgb 0x83 0xa5 0x98,
      blue = rgb 0x45 0x85 0x88,
      darkBlue = rgb 0x07 0x66 0x78,
      lightGreen = rgb 0xb8 0xbb 0x26,
      green = rgb 0x98 0x97 0x1a,
      darkGreen = rgb 0x79 0x74 0x0e,
      lightOrange = rgb 0xff 0xa0 0x7a,
      orange = rgb 0xd7 0x99 0x21,
      darkOrange = rgb 0xff 0x45 0x00,
      lightPink = rgb 0xff 0xb6 0xc1,
      pink = rgb 0xf3 0x86 0xcb,
      darkPink = rgb 0xff 0x14 0x93,
      lightPurple = rgb 0xf3 0x86 0xcb,
      purple = rgb 0xb1 0x62 0xd6,
      darkPurple = rgb 0x8f 0x3f 0x71,
      lightRed = rgb 0xfb 0x49 0x34,
      red = rgb 0xcc 0x24 0x1d,
      darkRed = rgb 0x9d 0x00 0x06,
      lightYellow = rgb 0xff 0xfa 0xcd,
      yellow = rgb 0xfa 0xbd 0x2f,
      darkYellow = rgb 0xb8 0x86 0x0b
    }

-- | The Monokai theme.
monokai :: Theme
monokai =
  Theme
    { black = rgb 0x27 0x28 0x22,
      gray = rgb 0x4f 0x4f 0x4f,
      white = rgb 0xf8 0xf8 0xf2,
      lightBlue = rgb 0x66 0xd9 0xef,
      blue = rgb 0x66 0xd9 0xef,
      darkBlue = rgb 0x00 0x5f 0x87,
      lightGreen = rgb 0xa6 0xe2 0x2e,
      green = rgb 0xa6 0xe2 0x2e,
      darkGreen = rgb 0x5f 0x87 0x00,
      lightOrange = rgb 0xff 0xa0 0x7a,
      orange = rgb 0xff 0xa0 0x7a,
      darkOrange = rgb 0xff 0x45 0x00,
      lightPink = rgb 0xff 0xb6 0xc1,
      pink = rgb 0xfe 0x81 0xff,
      darkPink = rgb 0xff 0x14 0x93,
      lightPurple = rgb 0xfe 0x81 0xff,
      purple = rgb 0xae 0x81 0xff,
      darkPurple = rgb 0x5f 0x00 0xaf,
      lightRed = rgb 0xff 0x61 0x88,
      red = rgb 0xf9 0x26 0x72,
      darkRed = rgb 0xd7 0x00 0x5f,
      lightYellow = rgb 0xff 0xfa 0xcd,
      yellow = rgb 0xe6 0xdb 0x74,
      darkYellow = rgb 0xb8 0x86 0x0b
    }

-- | The One Dark theme.
onedark :: Theme
onedark =
  Theme
    { black = rgb 0x2d 0x31 0x39,
      gray = rgb 0x4f 0x4f 0x4f,
      white = rgb 0xf8 0xf8 0xf8,
      lightBlue = rgb 0x61 0xaf 0xef,
      blue = rgb 0x61 0xaf 0xef,
      darkBlue = rgb 0x1b 0x4f 0x9c,
      lightGreen = rgb 0x98 0xc3 0x79,
      green = rgb 0x98 0xc3 0x79,
      darkGreen = rgb 0x4b 0x56 0x32,
      lightOrange = rgb 0xff 0xa0 0x7a,
      orange = rgb 0xff 0xa0 0x7a,
      darkOrange = rgb 0xff 0x45 0x00,
      lightPink = rgb 0xff 0xb6 0xc1,
      pink = rgb 0xf6 0x78 0xcd,
      darkPink = rgb 0xff 0x14 0x93,
      lightPurple = rgb 0xf6 0x78 0xcd,
      purple = rgb 0xc6 0x78 0xfd,
      darkPurple = rgb 0x5f 0x00 0xaf,
      lightRed = rgb 0xe0 0x6c 0x75,
      red = rgb 0xe0 0x6c 0x75,
      darkRed = rgb 0xbe 0x50 0x46,
      lightYellow = rgb 0xff 0xfa 0xcd,
      yellow = rgb 0xe5 0xc0 0x7b,
      darkYellow = rgb 0xb8 0x86 0x0b
    }

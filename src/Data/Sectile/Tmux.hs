{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module        : Data.Sectile.Tmux
-- Copyright     : Gautier DI FOLCO
-- License       : ISC
--
-- Maintainer    : Gautier DI FOLCO <foss@difolco.dev>
-- Stability     : Stable
-- Portability   : Portable
module Data.Sectile.Tmux
  ( Chunk (..),
    ChunkStyle (..),
    Colour (..),
    TerminalColour (..),
    Brightness (..),
    ConsoleIntensity (..),
    Underlining (..),
    Blinking (..),
    noStyle,
    chunkWidth,
    TerminalCapabilities (..),
    renderChunksUtf8BSBuilder,
    renderChunkStyleUtf8BSBuilder,
    parseAnsiChunks,
    renderColour,
  )
where

import qualified Data.ByteString.Builder as B
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Word (Word8)
import Numeric (showHex)

data TerminalColour = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White
  deriving (Show, Eq, Ord)

data Brightness = Bright | Dull
  deriving (Show, Eq, Ord)

data Colour
  = Colour8 Brightness TerminalColour
  | Colour24Bit Word8 Word8 Word8
  deriving (Show, Eq, Ord)

data ConsoleIntensity = BoldIntensity | FaintIntensity | NormalIntensity
  deriving (Show, Eq, Ord)

data Underlining = SingleUnderline | DoubleUnderline | NoUnderline
  deriving (Show, Eq, Ord)

data Blinking = SlowBlinking | RapidBlinking | NoBlinking
  deriving (Show, Eq, Ord)

data ChunkStyle = ChunkStyle
  { chunkStyleForeground :: Maybe Colour,
    chunkStyleBackground :: Maybe Colour,
    chunkStyleItalic :: Maybe Bool,
    chunkStyleStrikethrough :: Maybe Bool,
    chunkStyleSwapForegroundBackground :: Maybe Bool,
    chunkStyleConcealed :: Maybe Bool,
    chunkStyleOverlined :: Maybe Bool,
    chunkStyleConsoleIntensity :: Maybe ConsoleIntensity,
    chunkStyleUnderlining :: Maybe Underlining,
    chunkStyleBlinking :: Maybe Blinking,
    chunkStyleHyperlink :: Maybe Text
  }
  deriving (Show, Eq, Ord)

noStyle :: ChunkStyle
noStyle =
  ChunkStyle
    { chunkStyleForeground = Nothing,
      chunkStyleBackground = Nothing,
      chunkStyleItalic = Nothing,
      chunkStyleStrikethrough = Nothing,
      chunkStyleSwapForegroundBackground = Nothing,
      chunkStyleConcealed = Nothing,
      chunkStyleOverlined = Nothing,
      chunkStyleConsoleIntensity = Nothing,
      chunkStyleUnderlining = Nothing,
      chunkStyleBlinking = Nothing,
      chunkStyleHyperlink = Nothing
    }

data Chunk = Chunk
  { chunkText :: Text,
    chunkStyle :: ChunkStyle
  }
  deriving (Show, Eq, Ord)

chunkWidth :: Chunk -> Int
chunkWidth = T.length . chunkText

data TerminalCapabilities
  = WithoutColours
  | With8Colours
  | With8BitColours
  | With24BitColours
  deriving (Show, Eq, Ord)

parseAnsiChunks :: ChunkStyle -> Text -> (ChunkStyle, [Chunk])
parseAnsiChunks style txt = (style, [Chunk txt style])

renderChunksUtf8BSBuilder :: TerminalCapabilities -> [Chunk] -> B.Builder
renderChunksUtf8BSBuilder cap chunks = foldMap renderChunk chunks
  where
    renderChunk c =
      let txt = chunkText c
       in case renderChunkStyleUtf8BSBuilder cap (chunkStyle c) of
            Nothing -> B.byteString (T.encodeUtf8 txt)
            Just renderedStyle -> renderedStyle <> B.byteString (T.encodeUtf8 txt) <> "#[default]"

renderChunkStyleUtf8BSBuilder :: TerminalCapabilities -> ChunkStyle -> Maybe B.Builder
renderChunkStyleUtf8BSBuilder cap style =
  if cap == WithoutColours || null attrs
    then Nothing
    else Just $ "#[" <> B.byteString (T.encodeUtf8 $ T.intercalate "," attrs) <> "]"
  where
    fg = case chunkStyleForeground style of
      Nothing -> []
      Just col -> ["fg=" <> renderColour col]
    bg = case chunkStyleBackground style of
      Nothing -> []
      Just col -> ["bg=" <> renderColour col]
    bold = case chunkStyleConsoleIntensity style of
      Just BoldIntensity -> ["bold"]
      Just FaintIntensity -> ["dim"]
      _ -> []
    italic = case chunkStyleItalic style of
      Just True -> ["italics"]
      _ -> []
    underlined = case chunkStyleUnderlining style of
      Just SingleUnderline -> ["underscore"]
      Just DoubleUnderline -> ["underscore"]
      _ -> []
    blink = case chunkStyleBlinking style of
      Just SlowBlinking -> ["blink"]
      Just RapidBlinking -> ["blink"]
      _ -> []
    reverse' = case chunkStyleSwapForegroundBackground style of
      Just True -> ["reverse"]
      _ -> []
    hidden = case chunkStyleConcealed style of
      Just True -> ["hidden"]
      _ -> []
    strike = case chunkStyleStrikethrough style of
      Just True -> ["strikethrough"]
      _ -> []
    attrs = mconcat [fg, bg, bold, italic, underlined, blink, reverse', hidden, strike]

renderColour :: Colour -> Text
renderColour =
  \case
    Colour8 _ Black -> "black"
    Colour8 _ Red -> "red"
    Colour8 _ Green -> "green"
    Colour8 _ Yellow -> "yellow"
    Colour8 _ Blue -> "blue"
    Colour8 _ Magenta -> "magenta"
    Colour8 _ Cyan -> "cyan"
    Colour8 _ White -> "white"
    Colour24Bit r g b ->
      let hex = pad (showHex r "") <> pad (showHex g "") <> pad (showHex b "")
          pad s
            | length s == 1 = "0" <> s
            | otherwise = s
       in "#" <> T.pack hex

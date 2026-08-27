{-# LANGUAGE OverloadedStrings #-}

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
    parseAnsiChunks,
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
noStyle = ChunkStyle Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

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
      let style = chunkStyle c
          txt = chunkText c
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
       in if cap == WithoutColours || null attrs
            then B.byteString (T.encodeUtf8 txt)
            else "#[" <> B.byteString (T.encodeUtf8 $ T.intercalate "," attrs) <> "]" <> B.byteString (T.encodeUtf8 txt) <> "#[default]"

    renderColour :: Colour -> Text
    renderColour (Colour8 _ Black) = "black"
    renderColour (Colour8 _ Red) = "red"
    renderColour (Colour8 _ Green) = "green"
    renderColour (Colour8 _ Yellow) = "yellow"
    renderColour (Colour8 _ Blue) = "blue"
    renderColour (Colour8 _ Magenta) = "magenta"
    renderColour (Colour8 _ Cyan) = "cyan"
    renderColour (Colour8 _ White) = "white"
    renderColour (Colour24Bit r g b) =
      let hex = pad (showHex r "") <> pad (showHex g "") <> pad (showHex b "")
       in "#" <> T.pack hex

    pad s
      | length s == 1 = "0" <> s
      | otherwise = s

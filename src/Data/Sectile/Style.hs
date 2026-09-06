-- |
-- Module        : Data.Sectile.Style
-- Copyright     : Gautier DI FOLCO
-- License       : ISC
--
-- Maintainer    : Gautier DI FOLCO <foss@difolco.dev>
-- Stability     : Stable
-- Portability   : Portable
module Data.Sectile.Style
  ( -- * Style combinators
    between,
    changeStyle,
    forceStyle,

    -- * Style transformations
    resetStyle,
    swapForegroundBackgroundStyle,

    -- * Combinators
    warnIf,
    GradientSource (..),
    parseTextGradient,
    scaleGradient,
    ratioGradient,
    gradient,

    -- * Style optics
    styleItalic,
    styleStrikethrough,
    styleSwapForegroundBackground,
    styleConcealed,
    styleOverlined,
    styleConsoleIntensity,
    styleUnderlining,
    styleBlinking,
    styleForeground,
    styleBackground,
    styleHyperlink,
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Sectile.Tmux as Colour
import Data.Sectile.Types
import qualified Data.Text as T
import Data.Word (Word8)
import qualified Optics.Core as Optics

-- | Wrap a list of segments between a start and end segment.
--
-- Example:
--
-- > import Data.Sectile
-- >
-- > wrapped :: [Segment IO]
-- > wrapped = between (string "[") (string "]") [string "a", string "b"]
-- > -- Produces: [string "[", string "a", string "b", string "]"]
between :: Segment m -> Segment m -> [Segment m] -> [Segment m]
between start end ss = start : (ss <> [end])

-- | Modify the incoming style before it reaches a segment.
--
-- The style transformation is applied to the style passed *into* the segment,
-- but does not affect the rendered output retroactively.
--
-- Example:
--
-- > import Data.Sectile
-- > import qualified Data.Sectile.Tmux as Colour
-- >
-- > boldSegment :: Segment IO -> Segment IO
-- > boldSegment = changeStyle (\s -> s {Colour.chunkStyleConsoleIntensity = Just Colour.BoldIntensity})
changeStyle :: (Functor m) => (Colour.ChunkStyle -> Colour.ChunkStyle) -> Segment m -> Segment m
changeStyle c (Segment s) = Segment $ fmap transform s
  where
    transform action = do
      _ <- updateStyle c
      action

-- | Force a style transformation on all chunks in a segment's output.
--
-- Unlike 'changeStyle', this modifies every chunk in the rendered output,
-- the final style, and the explanation renderer.
--
-- Example:
--
-- > import Data.Sectile
-- > import qualified Data.Sectile.Tmux as Colour
-- >
-- > makeItalic :: Segment IO -> Segment IO
-- > makeItalic = forceStyle (\s -> s {Colour.chunkStyleItalic = Just True})
forceStyle :: (Functor m) => (Colour.ChunkStyle -> Colour.ChunkStyle) -> Segment m -> Segment m
forceStyle c (Segment s) = Segment $ fmap transform s
  where
    transform action = do
      formatted <- action
      _ <- updateStyle c
      pure
        formatted
          { rendered = updateChunk <$> formatted.rendered,
            explain = \renderSyle renderChunks ->
              formatted.explain renderSyle $ renderChunks . map updateChunk
          }
    updateChunk chunk = chunk {Colour.chunkStyle = c $ Colour.chunkStyle chunk}

-- | Reset a style to the default (no styling).
--
-- Example:
--
-- > import Data.Sectile
-- >
-- > plain :: Segment IO -> Segment IO
-- > plain = changeStyle resetStyle
resetStyle :: Colour.ChunkStyle -> Colour.ChunkStyle
resetStyle = const Colour.noStyle

-- | Swap foreground and background colours in a style.
--
-- Example:
--
-- > import Data.Sectile
-- >
-- > inverted :: Segment IO -> Segment IO
-- > inverted = forceStyle swapForegroundBackgroundStyle
swapForegroundBackgroundStyle :: Colour.ChunkStyle -> Colour.ChunkStyle
swapForegroundBackgroundStyle s =
  s
    { Colour.chunkStyleForeground = Colour.chunkStyleBackground s,
      Colour.chunkStyleBackground = Colour.chunkStyleForeground s
    }

-- | Apply a style if the segment text matches a predicate.
--
-- Example:
--
-- > import Data.Sectile
-- > import Data.Sectile.Style
-- > import qualified Data.Sectile.Tmux as Colour
-- > import qualified Data.Text as T
-- >
-- > alert :: Segment IO -> Segment IO
-- > alert = warnIf (\t -> "Error" `T.isInfixOf` t) (Colour.noStyle {Colour.chunkStyleForeground = Just (Colour.Colour8 Colour.Bright Colour.Red)})
warnIf :: (Functor m) => (T.Text -> Bool) -> Colour.ChunkStyle -> Segment m -> Segment m
warnIf p warnStyle (Segment s) = Segment $ fmap transform s
  where
    transform action = do
      formatted <- action
      let txt = mconcat $ map Colour.chunkText formatted.rendered
          applyWarn c = c {Colour.chunkStyle = warnStyle}
      if p txt
        then
          pure
            formatted
              { rendered = map applyWarn formatted.rendered,
                explain = \renderSyle renderChunks ->
                  formatted.explain renderSyle $ renderChunks . map applyWarn
              }
        else pure formatted

-- | A source of gradient input: extracts a value from the segment
-- bindings or the rendered text.
newtype GradientSource = GradientSource (HashMap.HashMap T.Text Aeson.Value -> T.Text -> Maybe Double)

-- | Build a 'GradientSource' by parsing the segment's rendered text.
parseTextGradient :: (T.Text -> Maybe Double) -> GradientSource
parseTextGradient f = GradientSource $ \_ txt -> f txt

-- | Build a 'GradientSource' from a numeric binding stored under @key@.
scaleGradient :: T.Text -> GradientSource
scaleGradient key = GradientSource $ \bnds _ ->
  case HashMap.lookup key bnds of
    Just (Aeson.Number n) -> Just (realToFrac n)
    _ -> Nothing

-- | Build a 'GradientSource' from the ratio of two numeric bindings.
ratioGradient :: T.Text -> T.Text -> GradientSource
ratioGradient k1 k2 = GradientSource $ \bnds _ ->
  case (HashMap.lookup k1 bnds, HashMap.lookup k2 bnds) of
    (Just (Aeson.Number n1), Just (Aeson.Number n2)) | n2 /= 0 -> Just (realToFrac (n1 / n2))
    _ -> Nothing

-- | Apply a color gradient based on a parsed value.
gradient ::
  (Functor m) =>
  (Colour.Colour -> Colour.ChunkStyle -> Colour.ChunkStyle) ->
  (Word8, Word8, Word8) ->
  (Word8, Word8, Word8) ->
  GradientSource ->
  Segment m ->
  Segment m
gradient applyColor (r1, g1, b1) (r2, g2, b2) source (Segment s) =
  Segment $ fmap transform s
  where
    transform action = do
      formatted <- action
      bnds <- currentBindings
      let txt = mconcat $ map Colour.chunkText formatted.rendered
      let GradientSource gradFn = source
      let mPct = gradFn bnds txt
      case mPct of
        Just pct -> do
          let p = max 0 (min 1 pct)
              r = round $ fromIntegral r1 * (1 - p) + fromIntegral r2 * p
              g = round $ fromIntegral g1 * (1 - p) + fromIntegral g2 * p
              b = round $ fromIntegral b1 * (1 - p) + fromIntegral b2 * p
              col = Colour.Colour24Bit r g b
              applyGrad = applyColor col
          _ <- updateStyle applyGrad
          let applyGradChunk chunk =
                chunk
                  { Colour.chunkStyle = applyGrad $ Colour.chunkStyle chunk
                  }
          pure
            formatted
              { rendered = map applyGradChunk formatted.rendered,
                explain = \renderSyle renderChunks ->
                  formatted.explain renderSyle $ renderChunks . map applyGradChunk
              }
        Nothing -> pure formatted

-- | Lens for the italic flag of a 'Colour.ChunkStyle'.
styleItalic :: Optics.Lens' Colour.ChunkStyle (Maybe Bool)
styleItalic = Optics.lens Colour.chunkStyleItalic (\s a -> s {Colour.chunkStyleItalic = a})

-- | Lens for the strikethrough flag of a 'Colour.ChunkStyle'.
styleStrikethrough :: Optics.Lens' Colour.ChunkStyle (Maybe Bool)
styleStrikethrough = Optics.lens Colour.chunkStyleStrikethrough (\s a -> s {Colour.chunkStyleStrikethrough = a})

-- | Lens for the swap-foreground-background flag of a 'Colour.ChunkStyle'.
styleSwapForegroundBackground :: Optics.Lens' Colour.ChunkStyle (Maybe Bool)
styleSwapForegroundBackground = Optics.lens Colour.chunkStyleSwapForegroundBackground (\s a -> s {Colour.chunkStyleSwapForegroundBackground = a})

-- | Lens for the concealed flag of a 'Colour.ChunkStyle'.
styleConcealed :: Optics.Lens' Colour.ChunkStyle (Maybe Bool)
styleConcealed = Optics.lens Colour.chunkStyleConcealed (\s a -> s {Colour.chunkStyleConcealed = a})

-- | Lens for the overlined flag of a 'Colour.ChunkStyle'.
styleOverlined :: Optics.Lens' Colour.ChunkStyle (Maybe Bool)
styleOverlined = Optics.lens Colour.chunkStyleOverlined (\s a -> s {Colour.chunkStyleOverlined = a})

-- | Lens for the console intensity of a 'Colour.ChunkStyle'.
styleConsoleIntensity :: Optics.Lens' Colour.ChunkStyle (Maybe Colour.ConsoleIntensity)
styleConsoleIntensity = Optics.lens Colour.chunkStyleConsoleIntensity (\s a -> s {Colour.chunkStyleConsoleIntensity = a})

-- | Lens for the underlining of a 'Colour.ChunkStyle'.
styleUnderlining :: Optics.Lens' Colour.ChunkStyle (Maybe Colour.Underlining)
styleUnderlining = Optics.lens Colour.chunkStyleUnderlining (\s a -> s {Colour.chunkStyleUnderlining = a})

-- | Lens for the blinking of a 'Colour.ChunkStyle'.
styleBlinking :: Optics.Lens' Colour.ChunkStyle (Maybe Colour.Blinking)
styleBlinking = Optics.lens Colour.chunkStyleBlinking (\s a -> s {Colour.chunkStyleBlinking = a})

-- | Lens for the foreground colour of a 'Colour.ChunkStyle'.
styleForeground :: Optics.Lens' Colour.ChunkStyle (Maybe Colour.Colour)
styleForeground = Optics.lens Colour.chunkStyleForeground (\s a -> s {Colour.chunkStyleForeground = a})

-- | Lens for the background colour of a 'Colour.ChunkStyle'.
styleBackground :: Optics.Lens' Colour.ChunkStyle (Maybe Colour.Colour)
styleBackground = Optics.lens Colour.chunkStyleBackground (\s a -> s {Colour.chunkStyleBackground = a})

-- | Lens for the hyperlink URL of a 'Colour.ChunkStyle'.
styleHyperlink :: Optics.Lens' Colour.ChunkStyle (Maybe T.Text)
styleHyperlink = Optics.lens Colour.chunkStyleHyperlink (\s a -> s {Colour.chunkStyleHyperlink = a})

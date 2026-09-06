-- |
-- Module        : Convert
-- Copyright     : Gautier DI FOLCO
-- License       : ISC
--
-- Maintainer    : Gautier DI FOLCO <foss@difolco.dev>
-- Stability     : Stable
-- Portability   : Portable
--
-- Convert Dhall DTOs to sectile library types.
module Convert
  ( convertBar,
  )
where

import Control.Concurrent.Async (mapConcurrently)
import Data.Char (isDigit)
import qualified Data.Sectile as Sectile
import qualified Data.Sectile.Display as Display
import qualified Data.Sectile.Style as Style
import qualified Data.Sectile.System.Linux as System
import qualified Data.Sectile.Tmux as Colour
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Word as Word
import qualified DhallTypes as S
import Numeric.Natural (Natural)
import qualified Optics.Core as Optics

-- | Convert a full bar configuration into a list of sectile segments.
convertBar :: S.BarConfig -> [Sectile.Segment IO]
convertBar cfg =
  let groupRows [] = []
      groupRows (x : xs) = case x.row of
        Nothing -> convertNode x : groupRows xs
        Just r ->
          let (rowNodes, rest) = span (\n -> n.row == Just r) (x : xs)
           in Sectile.row mapConcurrently Sectile.Isolating (mkName (T.pack $ "row-" ++ show r)) (map convertNode rowNodes) : groupRows rest
      segs = groupRows cfg.segments
   in case cfg.separator of
        Nothing -> segs
        Just sep ->
          let sepSeg = Sectile.string sep
           in intercalateSeg sepSeg segs

-- | Convert a single segment node.
convertNode :: S.SegmentNode -> Sectile.Segment IO
convertNode cfg =
  let seg = convertSegment cfg.segment
      withStyle = case cfg.style of
        Nothing -> seg
        Just s -> applyStyle s seg
      withDisplay = case cfg.display of
        Nothing -> withStyle
        Just d -> applyDisplay d withStyle
   in withDisplay

-- | Convert a single segment configuration to a library segment.
convertSegment :: S.Segment -> Sectile.Segment IO
convertSegment =
  \case
    S.String {..} ->
      Sectile.string text
    S.Shell {..} ->
      Sectile.sh (mkName name) (T.unpack command) Nothing
    S.Time {..} ->
      Sectile.time (mkName name) (T.unpack format)
    S.Volume {..} ->
      Sectile.volume (mkName name)
    S.Mpris {..} ->
      Sectile.mpris (mkName name)
    S.Git {..} ->
      Sectile.git (mkName name) (T.unpack path)
    S.HttpPoll {..} ->
      Sectile.httpPoll (mkName name) (T.unpack url)
    S.Uptime {..} ->
      System.uptime (mkName name)
    S.Memory {..} ->
      System.memory (mkName name)
    S.Load {..} ->
      System.load (mkName name)
    S.Cpu {..} ->
      System.cpu (mkName name)
    S.Disk {..} ->
      System.disk (mkName name) (T.unpack mountPoint)
    S.NetworkUp {..} ->
      System.networkUp (mkName name) interfaces
    S.NetworkDown {..} ->
      System.networkDown (mkName name) interfaces
    S.Battery {..} ->
      System.battery (mkName name) (T.unpack battery)
    S.Thermal {..} ->
      System.thermal (mkName name) (T.unpack zone)
    S.Wifi {..} ->
      System.wifi (mkName name) (T.unpack interface)

-- | Apply a style configuration to a segment.
applyStyle :: S.StyleConfig -> Sectile.Segment IO -> Sectile.Segment IO
applyStyle cfg seg =
  let parsePercent t =
        case T.splitOn "%" t of
          [] -> Nothing
          [_] -> Nothing
          (xs : _) ->
            let numStr = T.takeWhileEnd (\c -> c == '.' || isDigit c) xs
             in case reads (T.unpack numStr) of
                  [(d, "")] -> Just (d / 100.0)
                  _ -> Nothing
      parseLoad t =
        case reads (T.unpack t) of
          [(d, _)] -> Just (max 0 (min 1 (d / 4.0)))
          _ -> Nothing

      getParser =
        \case
          "percentage" -> parsePercent
          "load" -> parseLoad
          _ -> const Nothing

      getGradientSource = \case
        S.ParseText p -> Style.parseTextGradient (getParser p)
        S.Scale key -> Style.scaleGradient key
        S.Ratio k1 k2 -> Style.ratioGradient k1 k2

      applyColorConfig optic cfgVal =
        case cfgVal of
          Nothing -> id
          Just (S.Colour c) -> Style.forceStyle (Optics.set optic (Just (convertColour c)))
          Just (S.Gradient (S.GradientConfig f t src)) ->
            let S.ColourRecord r1 g1 b1 = f
                S.ColourRecord r2 g2 b2 = t
             in Style.gradient
                  (Optics.set optic . Just)
                  (fromIntegral r1, fromIntegral g1, fromIntegral b1)
                  (fromIntegral r2, fromIntegral g2, fromIntegral b2)
                  (getGradientSource src)

      withFg = applyColorConfig Style.styleForeground cfg.foreground
      withBg = applyColorConfig Style.styleBackground cfg.background

      applyOptic optic converter =
        maybe id (Style.forceStyle . Optics.set optic . Just . converter)

      withBold = case cfg.bold of
        Nothing -> id
        Just True -> Style.forceStyle (Optics.set Style.styleConsoleIntensity (Just Colour.BoldIntensity))
        Just False -> id

      withItalic = applyOptic Style.styleItalic id cfg.italic
      withStrikethrough = applyOptic Style.styleStrikethrough id cfg.strikethrough
      withSwap = applyOptic Style.styleSwapForegroundBackground id cfg.swapForegroundBackground
      withConcealed = applyOptic Style.styleConcealed id cfg.concealed
      withOverlined = applyOptic Style.styleOverlined id cfg.overlined
      withConsoleIntensity = applyOptic Style.styleConsoleIntensity convertConsoleIntensity cfg.consoleIntensity
      withUnderlining = applyOptic Style.styleUnderlining convertUnderlining cfg.underlining
      withBlinking = applyOptic Style.styleBlinking convertBlinking cfg.blinking
      withHyperlink = applyOptic Style.styleHyperlink id cfg.hyperlink
   in withHyperlink $ withBlinking $ withUnderlining $ withConsoleIntensity $ withOverlined $ withConcealed $ withSwap $ withStrikethrough $ withItalic $ withBold $ withBg $ withFg seg

-- | Apply a display transformation to a segment.
applyDisplay :: S.DisplayConfig -> Sectile.Segment IO -> Sectile.Segment IO
applyDisplay =
  \case
    S.NoTransform -> id
    S.TakeStart {..} -> Display.takeStart (fromIntegral width)
    S.TakeEnd {..} -> Display.takeEnd (fromIntegral width)
    S.PadStart {..} -> Display.padStart (fromIntegral width)
    S.PadEnd {..} -> Display.padEnd (fromIntegral width)
    S.FixedSizeStart {..} -> Display.fixedSizeStart (fromIntegral width)
    S.FixedSizeEnd {..} -> Display.fixedSizeEnd (fromIntegral width)
    S.ProgressBar {..} -> Display.progressBar (fromIntegral width)
    S.Marquee {..} -> Display.marquee (fromIntegral width) (fromIntegral tickSeconds)
    S.Reformat {..} -> Sectile.reformat (convertPropagatingStyle propagatingStyle) format

convertPropagatingStyle :: Maybe S.PropagatingStyle -> Sectile.PropagatingStyle
convertPropagatingStyle =
  \case
    Nothing -> Sectile.PropagateInner
    Just s ->
      case s of
        S.Reset -> Sectile.Reset
        S.PropagateIncoming -> Sectile.PropagateIncoming
        S.PropagateInner -> Sectile.PropagateInner

-- | Convert a Dhall colour to a safe-coloured-text colour.
convertColour :: S.Colour -> Colour.Colour
convertColour c = Colour.Colour24Bit (toW8 c.r) (toW8 c.g) (toW8 c.b)
  where
    toW8 :: Natural -> Word.Word8
    toW8 = fromIntegral . min 255

convertConsoleIntensity :: S.ConsoleIntensity -> Colour.ConsoleIntensity
convertConsoleIntensity =
  \case
    S.BoldIntensity -> Colour.BoldIntensity
    S.FaintIntensity -> Colour.FaintIntensity
    S.NormalIntensity -> Colour.NormalIntensity

convertUnderlining :: S.Underlining -> Colour.Underlining
convertUnderlining =
  \case
    S.SingleUnderline -> Colour.SingleUnderline
    S.DoubleUnderline -> Colour.DoubleUnderline
    S.NoUnderline -> Colour.NoUnderline

convertBlinking :: S.Blinking -> Colour.Blinking
convertBlinking =
  \case
    S.SlowBlinking -> Colour.SlowBlinking
    S.RapidBlinking -> Colour.RapidBlinking
    S.NoBlinking -> Colour.NoBlinking

-- | Create a segment name from text.
mkName :: T.Text -> Sectile.Name
mkName = Sectile.Name . T.encodeUtf8Builder

-- | Intersperse a separator segment between segments.
intercalateSeg :: Sectile.Segment IO -> [Sectile.Segment IO] -> [Sectile.Segment IO]
intercalateSeg sep =
  \case
    [] -> []
    [x] -> [x]
    (x : xs) -> x : sep : intercalateSeg sep xs

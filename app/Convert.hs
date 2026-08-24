-- | Convert Dhall DTOs to sectile library types.
module Convert
  ( convertBar,
  )
where

import Control.Concurrent.Async (mapConcurrently)
import qualified Data.Sectile as Sectile
import qualified Data.Sectile.Display as Display
import qualified Data.Sectile.Style as Style
import qualified Data.Sectile.System.Linux as System
import qualified Data.Sectile.Themes as Themes
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Word as Word
import qualified DhallTypes as S
import Numeric.Natural (Natural)
import qualified Text.Colour.Chunk as Colour
import qualified Text.Colour.Code as Colour

-- | Convert a full bar configuration into a list of sectile segments.
convertBar :: S.BarConfig -> [Sectile.Segment IO]
convertBar cfg =
  let segs = convertNode <$> cfg.segments
   in case cfg.separator of
        Nothing -> segs
        Just sep ->
          let sepSeg = Sectile.string sep
           in intercalateSeg sepSeg segs

-- | Convert a segment node configuration to a styled library segment.
convertNode :: S.SegmentNode -> Sectile.Segment IO
convertNode (S.SegmentNode {..}) =
  let base = convertSegment segment
      styled = maybe id applyStyle style base
   in maybe id applyDisplay display styled

-- | Convert a single segment configuration to a library segment.
convertSegment :: S.SegmentConfig -> Sectile.Segment IO
convertSegment = \case
  S.StringSegment {..} ->
    Sectile.string text
  S.ShellSegment {..} ->
    Sectile.sh (mkName name) (T.unpack command) Nothing
  S.TimeSegment {..} ->
    Sectile.time (mkName name) (T.unpack format)
  S.VolumeSegment {..} ->
    Sectile.volume (mkName name)
  S.MprisSegment {..} ->
    Sectile.mpris (mkName name)
  S.GitSegment {..} ->
    Sectile.git (mkName name) (T.unpack path)
  S.HttpPollSegment {..} ->
    Sectile.httpPoll (mkName name) (T.unpack url)
  S.UptimeSegment {..} ->
    System.uptime (mkName name)
  S.MemorySegment {..} ->
    System.memory (mkName name)
  S.LoadSegment {..} ->
    System.load (mkName name)
  S.CpuSegment {..} ->
    System.cpu (mkName name)
  S.DiskSegment {..} ->
    System.disk (mkName name) (T.unpack mountPoint)
  S.NetworkUpSegment {..} ->
    System.networkUp (mkName name) interface
  S.NetworkDownSegment {..} ->
    System.networkDown (mkName name) interface
  S.BatterySegment {..} ->
    System.battery (mkName name) (T.unpack battery)
  S.ThermalSegment {..} ->
    System.thermal (mkName name) (T.unpack zone)
  S.WifiSegment {..} ->
    System.wifi (mkName name) (T.unpack interface)


-- | Apply a style configuration to a segment.
applyStyle :: S.StyleConfig -> Sectile.Segment IO -> Sectile.Segment IO
applyStyle cfg seg =
  let withFg = case cfg.foreground of
        Nothing -> id
        Just c -> Style.forceStyle (\s -> s {Colour.chunkStyleForeground = Just (convertColour c)})
      withBg = case cfg.background of
        Nothing -> id
        Just c -> Style.forceStyle (\s -> s {Colour.chunkStyleBackground = Just (convertColour c)})
      withBold = case cfg.bold of
        Nothing -> id
        Just True -> Style.forceStyle (\s -> s {Colour.chunkStyleConsoleIntensity = Just Colour.BoldIntensity})
        Just False -> id
      withItalic = case cfg.italic of
        Nothing -> id
        Just b -> Style.forceStyle (\s -> s {Colour.chunkStyleItalic = Just b})
      withTheme = case (cfg.theme, cfg.themeForeground, cfg.themeBackground) of
        (Just themeName, Just fgName, Just bgName) ->
          let t = resolveTheme themeName
              fg = resolveThemeColour t fgName
              bg = resolveThemeColour t bgName
           in Style.forceStyle (Themes.themeStyle fg bg)
        _ -> id
   in withTheme $ withItalic $ withBold $ withBg $ withFg seg

-- | Apply a display transformation to a segment.
applyDisplay :: S.DisplayConfig -> Sectile.Segment IO -> Sectile.Segment IO
applyDisplay = \case
  S.NoTransform -> id
  S.TakeStart {..} -> Display.takeStart (fromIntegral width)
  S.TakeEnd {..} -> Display.takeEnd (fromIntegral width)
  S.PadStart {..} -> Display.padStart (fromIntegral width)
  S.PadEnd {..} -> Display.padEnd (fromIntegral width)
  S.FixedSizeStart {..} -> Display.fixedSizeStart (fromIntegral width)
  S.FixedSizeEnd {..} -> Display.fixedSizeEnd (fromIntegral width)
  S.ProgressBar {..} -> Display.progressBar (fromIntegral width)
  S.Marquee {..} -> Display.marquee (fromIntegral width) (fromIntegral tickSeconds)

-- | Convert a Dhall colour to a safe-coloured-text colour.
convertColour :: S.Colour -> Colour.Colour
convertColour c = Colour.Colour24Bit (toW8 c.r) (toW8 c.g) (toW8 c.b)
  where
    toW8 :: Natural -> Word.Word8
    toW8 = fromIntegral . min 255

-- | Resolve a theme name to a theme value.
resolveTheme :: S.ThemeName -> Themes.Theme
resolveTheme tn = case tn.getThemeName of
  "catppuccin" -> Themes.catppuccin
  "gruvbox" -> Themes.gruvbox
  "monokai" -> Themes.monokai
  "onedark" -> Themes.onedark
  _ -> Themes.defaultTheme

-- | Resolve a colour name from a theme.
resolveThemeColour :: Themes.Theme -> T.Text -> Colour.Colour
resolveThemeColour t = \case
  "black" -> t.black
  "gray" -> t.gray
  "white" -> t.white
  "lightBlue" -> t.lightBlue
  "blue" -> t.blue
  "darkBlue" -> t.darkBlue
  "lightGreen" -> t.lightGreen
  "green" -> t.green
  "darkGreen" -> t.darkGreen
  "lightOrange" -> t.lightOrange
  "orange" -> t.orange
  "darkOrange" -> t.darkOrange
  "lightPink" -> t.lightPink
  "pink" -> t.pink
  "darkPink" -> t.darkPink
  "lightPurple" -> t.lightPurple
  "purple" -> t.purple
  "darkPurple" -> t.darkPurple
  "lightRed" -> t.lightRed
  "red" -> t.red
  "darkRed" -> t.darkRed
  "lightYellow" -> t.lightYellow
  "yellow" -> t.yellow
  "darkYellow" -> t.darkYellow
  _ -> t.white

-- | Create a segment name from text.
mkName :: T.Text -> Sectile.Name
mkName = Sectile.Name . T.encodeUtf8Builder

-- | Intersperse a separator segment between segments.
intercalateSeg :: Sectile.Segment IO -> [Sectile.Segment IO] -> [Sectile.Segment IO]
intercalateSeg _ [] = []
intercalateSeg _ [x] = [x]
intercalateSeg sep (x : xs) = x : sep : intercalateSeg sep xs

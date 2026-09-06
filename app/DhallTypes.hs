{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

-- |
-- Module        : DhallTypes
-- Copyright     : Gautier DI FOLCO
-- License       : ISC
--
-- Maintainer    : Gautier DI FOLCO <foss@difolco.dev>
-- Stability     : Stable
-- Portability   : Portable
--
-- Dhall-compatible DTOs for sectile configuration.
-- These types mirror the sectile library types but derive 'FromDhall'
-- for configuration file parsing.
module DhallTypes
  ( -- * Segment configuration
    Segment (..),
    SegmentNode (..),

    -- * Style configuration
    Colour (..),
    ColourConfig (..),
    ConsoleIntensity (..),
    Underlining (..),
    Blinking (..),
    StyleConfig (..),

    -- * Display configuration
    DisplayConfig (..),
    PropagatingStyle (..),

    -- * Theme configuration
    ThemeName (..),

    -- * Top-level configuration
    BarConfig (..), GradientConfig(..), GradientSourceConfig(..),
  )
where

import Data.String (IsString)
import Dhall

-- | A colour specified as 24-bit RGB components.
data Colour = ColourRecord
  { r :: Natural,
    g :: Natural,
    b :: Natural
  }
  deriving stock (Eq, Show, Generic)

deriving anyclass instance FromDhall Colour

-- | Data for gradient configuration.
data GradientSourceConfig
  = ParseText { parser :: Text }
  | Scale { key :: Text }
  | Ratio { k1 :: Text, k2 :: Text }
  deriving stock (Eq, Show, Generic)

deriving anyclass instance FromDhall GradientSourceConfig

data GradientConfig = GradientConfig
  { from :: Colour,
    to :: Colour,
    source :: GradientSourceConfig
  }
  deriving stock (Eq, Show, Generic)

deriving anyclass instance FromDhall GradientConfig

data ConsoleIntensity = BoldIntensity | FaintIntensity | NormalIntensity deriving stock (Eq, Show, Generic)
deriving anyclass instance FromDhall ConsoleIntensity

data Underlining = SingleUnderline | DoubleUnderline | NoUnderline deriving stock (Eq, Show, Generic)
deriving anyclass instance FromDhall Underlining

data Blinking = SlowBlinking | RapidBlinking | NoBlinking deriving stock (Eq, Show, Generic)
deriving anyclass instance FromDhall Blinking

data ColourConfig = Colour Colour | Gradient GradientConfig
  deriving stock (Eq, Show, Generic)
deriving anyclass instance FromDhall ColourConfig

-- | Style configuration for a segment.
data StyleConfig = StyleConfig
  { foreground :: Maybe ColourConfig,
    background :: Maybe ColourConfig,
    bold :: Maybe Bool,
    italic :: Maybe Bool,
    strikethrough :: Maybe Bool,
    swapForegroundBackground :: Maybe Bool,
    concealed :: Maybe Bool,
    overlined :: Maybe Bool,
    consoleIntensity :: Maybe ConsoleIntensity,
    underlining :: Maybe Underlining,
    blinking :: Maybe Blinking,
    hyperlink :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)

deriving anyclass instance FromDhall StyleConfig

-- | Named theme selection.
newtype ThemeName = ThemeName {getThemeName :: Text}
  deriving stock (Generic)
  deriving newtype (Eq, Ord, Show, IsString, FromDhall)

data PropagatingStyle
  = Reset
  | PropagateIncoming
  | PropagateInner
  deriving stock (Eq, Show, Generic)

deriving anyclass instance FromDhall PropagatingStyle

-- | Display transformation configuration.
data DisplayConfig
  = NoTransform
  | TakeStart {width :: Natural}
  | TakeEnd {width :: Natural}
  | PadStart {width :: Natural}
  | PadEnd {width :: Natural}
  | FixedSizeStart {width :: Natural}
  | FixedSizeEnd {width :: Natural}
  | ProgressBar {width :: Natural}
  | Marquee {width :: Natural, tickSeconds :: Natural}
  | Reformat {propagatingStyle :: Maybe PropagatingStyle, format :: Text}
  deriving stock (Eq, Show, Generic)

deriving anyclass instance FromDhall DisplayConfig

-- | A segment in the status bar configuration.
data Segment
  = String {text :: Text}
  | Shell {name :: Text, command :: Text}
  | Time {name :: Text, format :: Text}
  | Volume {name :: Text}
  | Mpris {name :: Text}
  | Git {name :: Text, path :: Text}
  | HttpPoll {name :: Text, url :: Text}
  | Uptime {name :: Text}
  | Memory {name :: Text}
  | Load {name :: Text}
  | Cpu {name :: Text}
  | Disk {name :: Text, mountPoint :: Text}
  | NetworkUp {name :: Text, interfaces :: [Text]}
  | NetworkDown {name :: Text, interfaces :: [Text]}
  | Battery {name :: Text, battery :: Text}
  | Thermal {name :: Text, zone :: Text}
  | Wifi {name :: Text, interface :: Text}
  deriving stock (Eq, Show, Generic)

deriving anyclass instance FromDhall Segment

-- | A segment with its styling and display configuration.
data SegmentNode = SegmentNode
  { segment :: Segment,
    style :: Maybe StyleConfig,
    display :: Maybe DisplayConfig,
    row :: Maybe Natural
  }
  deriving stock (Eq, Show, Generic)

deriving anyclass instance FromDhall SegmentNode

-- | Top-level bar configuration.
data BarConfig = BarConfig
  { segments :: [SegmentNode],
    separator :: Maybe Text,
    theme :: Maybe ThemeName
  }
  deriving stock (Eq, Show, Generic)

deriving anyclass instance FromDhall BarConfig

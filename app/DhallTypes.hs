{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

-- | Dhall-compatible DTOs for sectile configuration.
-- These types mirror the sectile library types but derive 'FromDhall'
-- for configuration file parsing.
module DhallTypes
  ( -- * Segment configuration
    Segment (..),
    SegmentNode (..),

    -- * Style configuration
    Colour (..),
    StyleConfig (..),

    -- * Display configuration
    DisplayConfig (..),

    -- * Theme configuration
    ThemeName (..),

    -- * Top-level configuration
    BarConfig (..), GradientConfig(..),
  )
where

import Data.String (IsString)
import Dhall

-- | A colour specified as 24-bit RGB components.
data Colour = Colour
  { r :: Natural,
    g :: Natural,
    b :: Natural
  }
  deriving stock (Eq, Show, Generic)

deriving anyclass instance FromDhall Colour

-- | Data for gradient configuration.
data GradientConfig = GradientConfig
  { from :: Colour,
    to :: Colour,
    parser :: Text
  }
  deriving stock (Eq, Show, Generic)

deriving anyclass instance FromDhall GradientConfig

-- | Style configuration for a segment.
data StyleConfig = StyleConfig
  { foreground :: Maybe Colour,
    background :: Maybe Colour,
    bold :: Maybe Bool,
    italic :: Maybe Bool,
    theme :: Maybe ThemeName,
    themeForeground :: Maybe Text,
    themeBackground :: Maybe Text,
    gradientFg :: Maybe GradientConfig,
    gradientBg :: Maybe GradientConfig
  }
  deriving stock (Eq, Show, Generic)

deriving anyclass instance FromDhall StyleConfig

-- | Named theme selection.
newtype ThemeName = ThemeName {getThemeName :: Text}
  deriving stock (Generic)
  deriving newtype (Eq, Ord, Show, IsString, FromDhall)

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

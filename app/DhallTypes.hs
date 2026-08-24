{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

-- | Dhall-compatible DTOs for sectile configuration.
-- These types mirror the sectile library types but derive 'FromDhall'
-- for configuration file parsing.
module DhallTypes
  ( -- * Segment configuration
    SegmentConfig (..),

    -- * Style configuration
    Colour (..),
    StyleConfig (..),

    -- * Display configuration
    DisplayConfig (..),

    -- * Theme configuration
    ThemeName (..),

    -- * Top-level configuration
    BarConfig (..),
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

-- | Style configuration for a segment.
data StyleConfig = StyleConfig
  { foreground :: Maybe Colour,
    background :: Maybe Colour,
    bold :: Maybe Bool,
    italic :: Maybe Bool,
    theme :: Maybe ThemeName,
    themeForeground :: Maybe Text,
    themeBackground :: Maybe Text
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
data SegmentConfig
  = StringSegment {text :: Text}
  | ShellSegment {name :: Text, command :: Text}
  | TimeSegment {name :: Text, format :: Text}
  | VolumeSegment {name :: Text}
  | MprisSegment {name :: Text}
  | GitSegment {name :: Text, path :: Text}
  | HttpPollSegment {name :: Text, url :: Text}
  | UptimeSegment {name :: Text}
  | MemorySegment {name :: Text}
  | LoadSegment {name :: Text}
  | CpuSegment {name :: Text}
  | DiskSegment {name :: Text, mountPoint :: Text}
  | NetworkUpSegment {name :: Text, interface :: Text}
  | NetworkDownSegment {name :: Text, interface :: Text}
  | BatterySegment {name :: Text, battery :: Text}
  | ThermalSegment {name :: Text, zone :: Text}
  | WifiSegment {name :: Text, interface :: Text}
  | RowSegment
      { name :: Text,
        segments :: [SegmentConfig],
        style :: Maybe StyleConfig,
        display :: Maybe DisplayConfig
      }
  deriving stock (Eq, Show, Generic)

deriving anyclass instance FromDhall SegmentConfig

-- | Top-level bar configuration.
data BarConfig = BarConfig
  { segments :: [SegmentConfig],
    separator :: Maybe Text,
    theme :: Maybe ThemeName
  }
  deriving stock (Eq, Show, Generic)

deriving anyclass instance FromDhall BarConfig

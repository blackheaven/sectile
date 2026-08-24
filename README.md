# sectile

Composable status line builder.

> Sectile: Derived from the latin _opus sectile_ (literally "cut work").
> Unlike standard mosaics that use uniform square tiles, this technique uses materials cut into specific, irregular shapes to fit a design.

## Overview

`sectile` provides a composable, type-safe way to build terminal status lines
in Haskell. Segments are the building blocks -- each one produces styled,
ANSI-aware terminal output that can be combined, transformed, and debugged.

### Features

- Composable segments with style propagation
- Tmux status bar formatting sequence rendering
- Shell command execution with error handling
- Linux system monitoring (uptime, memory, CPU, disk, network)
- Display transformations (truncation, padding, fixed-size, regex rewriting)
- Style optics (italic, bold, foreground, background, etc.)
- Predefined colour themes (catppuccin, gruvbox, monokai, onedark)
- Explanation/debug tree for segment introspection

## Modules

| Module | Description |
|---|---|
| `Data.Sectile` | Re-exports for convenience |
| `Data.Sectile.Types` | Core types (`Segment`, `Formatted`, `Name`, `SegmentsRunner`) |
| `Data.Sectile.Runners` | `renderSegment`, `explainSegment` |
| `Data.Sectile.Segments` | Segment constructors (`string`, `row`, `sh`, `time`) |
| `Data.Sectile.Style` | Style combinators and optics |
| `Data.Sectile.Themes` | Predefined colour themes (catppuccin, gruvbox, monokai, onedark) |
| `Data.Sectile.Display` | Display transformations (truncation, padding, regex) |
| `Data.Sectile.System.Linux` | Linux `/proc` system segments |

## Usage

### Basic example

```haskell
import Data.Sectile
import qualified Data.ByteString.Builder as B
import qualified Data.Sectile.Tmux as Colour
import System.IO (stdout)

main :: IO ()
main = do
  let status =
        row mapM "status" $
          between (string " [") (string "] ") $
            [ time "clock" "%H:%M",
              string " | ",
              sh "host" "hostname" Nothing
            ]
  output <- renderSegment Colour.With8Colours status
  B.hPutBuilder stdout output
```

### System monitoring

```haskell
import Data.Sectile

sysBar :: Segment IO
sysBar =
  row mapM "sys" $
    between (string " ") (string " ") $
      [ uptime "up",
        string " | ",
        memory "mem",
        string " | ",
        load "load",
        string " | ",
        cpu "cpu"
      ]
```

### Display transformations

```haskell
import Data.Sectile
import Data.Sectile.Display

-- Fixed-width hostname, right-aligned
fixedHost :: Segment IO
fixedHost = fixedSizeStart 15 (sh "host" "hostname" Nothing)

-- Truncate long output
shortPath :: Segment IO
shortPath = takeEnd 30 (sh "pwd" "pwd" Nothing)
```

### Style manipulation

```haskell
import Data.Sectile
import Data.Sectile.Style
import qualified Optics.Core as Optics
import qualified Data.Sectile.Tmux as Colour

-- Swap foreground/background
inverted :: Segment IO -> Segment IO
inverted = forceStyle swapForegroundBackgroundStyle

-- Set italic via optics
italic :: Segment IO -> Segment IO
italic = forceStyle (Optics.set styleItalic (Just True))
```

### Themes

Predefined colour palettes inspired by [tmux2k](https://github.com/2KAbhishek/tmux2k).
Available themes: `defaultTheme`, `catppuccin`, `gruvbox`, `monokai`, `onedark`.

```haskell
import Data.Sectile
import Data.Sectile.Style

-- Apply catppuccin colours to a segment
themed :: Segment IO
themed =
  let t = catppuccin
   in forceStyle (themeStyle t.blue t.black) (string "hello")

-- Mix theme colours across segments
themedBar :: Segment IO
themedBar =
  let t = gruvbox
   in row mapM "bar" $
        [ forceStyle (themeStyle t.green t.black) (string " git "),
          forceStyle (themeStyle t.yellow t.black) (string " cpu "),
          forceStyle (themeStyle t.red t.black) (string " mem ")
        ]
```

### Debugging

```haskell
import Data.Sectile
import qualified Data.ByteString.Builder as B
import qualified Data.Sectile.Tmux as Colour
import System.IO (stdout)

main :: IO ()
main = do
  let seg = row mapM "debug" [string "hello", string " world"]
  explanation <- explainSegment Colour.WithoutColours seg
  B.hPutBuilder stdout explanation
```

## Platform support

- The core library (`Data.Sectile`, `Data.Sectile.Segments`, etc.) is portable.
- `Data.Sectile.System.Linux` is **Linux-only** (reads `/proc` filesystem).
  On failure, each system segment displays `"Error on <name>"`.

## Executable / CLI

`sectile` includes an executable (`sectile`) that can read a Dhall configuration file and render the status line directly.

You can inspect the Dhall format:
```bash
sectile dump-dhall-format
```

And run a configuration:
```bash
sectile render --config examples/simple.dhall --24bit-colours
```

Or explain the segment layout of a configuration:
```bash
sectile explain --config examples/simple.dhall
```

### Dhall Configuration Example

```dhall
let S = ./format.dhall

in  S.BarConfig::{
    , segments =
      [ S.SegmentConfig.Type.TimeSegment { name = "clock", format = "%H:%M" }
      , S.SegmentConfig.Type.StringSegment { text = " | " }
      , S.SegmentConfig.Type.ShellSegment
          { name = "host", command = "hostname" }
      ]
    , separator = None Text
    }
```

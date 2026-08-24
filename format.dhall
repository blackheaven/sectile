let Colour = { Type = { r : Natural, g : Natural, b : Natural }, default = {=} }

let StyleConfig =
      { Type =
          { foreground : Optional Colour.Type
          , background : Optional Colour.Type
          , bold : Optional Bool
          , italic : Optional Bool
          , theme : Optional Text
          , themeForeground : Optional Text
          , themeBackground : Optional Text
          }
      , default =
        { foreground = None Colour.Type
        , background = None Colour.Type
        , bold = None Bool
        , italic = None Bool
        , theme = None Text
        , themeForeground = None Text
        , themeBackground = None Text
        }
      }

let DisplayConfig =
      { Type =
          < NoTransform
          | TakeStart : { width : Natural }
          | TakeEnd : { width : Natural }
          | PadStart : { width : Natural }
          | PadEnd : { width : Natural }
          | FixedSizeStart : { width : Natural }
          | FixedSizeEnd : { width : Natural }
          | ProgressBar : { width : Natural }
          | Marquee : { width : Natural, tickSeconds : Natural }
          >
      , default = {=}
      }

let SegmentConfig =
      { Type =
          < StringSegment : { text : Text }
          | ShellSegment : { name : Text, command : Text }
          | TimeSegment : { name : Text, format : Text }
          | VolumeSegment : { name : Text }
          | MprisSegment : { name : Text }
          | GitSegment : { name : Text, path : Text }
          | HttpPollSegment : { name : Text, url : Text }
          | UptimeSegment : { name : Text }
          | MemorySegment : { name : Text }
          | LoadSegment : { name : Text }
          | CpuSegment : { name : Text }
          | DiskSegment : { name : Text, mountPoint : Text }
          | NetworkUpSegment : { name : Text, interface : Text }
          | NetworkDownSegment : { name : Text, interface : Text }
          | BatterySegment : { name : Text, battery : Text }
          | ThermalSegment : { name : Text, zone : Text }
          | WifiSegment : { name : Text, interface : Text }

          >
      , default = {=}
      }

let SegmentNode =
      { Type =
          { segment : SegmentConfig.Type
          , style : Optional StyleConfig.Type
          , display : Optional DisplayConfig.Type
          }
      , default = { style = None StyleConfig.Type, display = None DisplayConfig.Type }
      }

let BarConfig =
      { Type =
          { segments : List SegmentNode.Type
          , separator : Optional Text
          , theme : Optional Text
          }
      , default =
        { separator = None Text
        , theme = None Text
        }
      }

in  { Colour
    , StyleConfig
    , DisplayConfig
    , SegmentConfig
    , SegmentNode
    , BarConfig
    }

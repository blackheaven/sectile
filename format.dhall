let Colour = { Type = { r : Natural, g : Natural, b : Natural }, default = {=} }

let GradientSourceConfig =
      < ParseText : { parser : Text }
      | Scale : { key : Text }
      | Ratio : { k1 : Text, k2 : Text }
      >

let GradientConfig =
      { Type = { from : Colour.Type, to : Colour.Type, source : GradientSourceConfig }
      , default = {=}
      }

let StyleConfig =
      { Type =
          { foreground : Optional Colour.Type
          , background : Optional Colour.Type
          , bold : Optional Bool
          , italic : Optional Bool
          , theme : Optional Text
          , themeForeground : Optional Text
          , themeBackground : Optional Text
          , gradientFg : Optional GradientConfig.Type
          , gradientBg : Optional GradientConfig.Type
          }
      , default =
        { foreground = None Colour.Type
        , background = None Colour.Type
        , bold = None Bool
        , italic = None Bool
        , theme = None Text
        , themeForeground = None Text
        , themeBackground = None Text
        , gradientFg = None GradientConfig.Type
        , gradientBg = None GradientConfig.Type
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
          | Reformat : { format : Text }
          >
      , default = {=}
      }

let Segment =
      { Type =
          < String : { text : Text }
          | Shell : { name : Text, command : Text }
          | Time : { name : Text, format : Text }
          | Volume : { name : Text }
          | Mpris : { name : Text }
          | Git : { name : Text, path : Text }
          | HttpPoll : { name : Text, url : Text }
          | Uptime : { name : Text }
          | Memory : { name : Text }
          | Load : { name : Text }
          | Cpu : { name : Text }
          | Disk : { name : Text, mountPoint : Text }
          | NetworkUp : { name : Text, interfaces : List Text }
          | NetworkDown : { name : Text, interfaces : List Text }
          | Battery : { name : Text, battery : Text }
          | Thermal : { name : Text, zone : Text }
          | Wifi : { name : Text, interface : Text }
          >
      , default = {=}
      }

let SegmentNode =
      { Type =
          { segment : Segment.Type
          , style : Optional StyleConfig.Type
          , display : Optional DisplayConfig.Type
          , row : Optional Natural
          }
      , default = { style = None StyleConfig.Type, display = None DisplayConfig.Type, row = None Natural }
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
    , GradientSourceConfig
    , GradientConfig
    , StyleConfig
    , DisplayConfig
    , Segment
    , SegmentNode
    , BarConfig
    }

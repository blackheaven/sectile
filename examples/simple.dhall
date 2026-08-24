let S = ../format.dhall

in  S.BarConfig::{
    , segments =
      [ S.SegmentConfig.Type.TimeSegment { name = "clock", format = "%H:%M" }
      , S.SegmentConfig.Type.StringSegment { text = " | " }
      , S.SegmentConfig.Type.ShellSegment
          { name = "host", command = "hostname" }
      ]
    , separator = None Text
    }

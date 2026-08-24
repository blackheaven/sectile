let S = ../format.dhall

in  S.BarConfig::{
    , segments =
      [ S.SegmentConfig.Type.RowSegment
          { name = "left"
          , segments =
            [ S.SegmentConfig.Type.TimeSegment
                { name = "clock", format = "%H:%M:%S" }
            , S.SegmentConfig.Type.StringSegment { text = " " }
            , S.SegmentConfig.Type.TimeSegment
                { name = "date", format = "%Y-%m-%d" }
            ]
          , style = Some
              S.StyleConfig::{
              , theme = Some "catppuccin"
              , themeForeground = Some "blue"
              , themeBackground = Some "black"
              }
          , display = Some
              (S.DisplayConfig.Type.FixedSizeEnd { width = 20 })
          }
      , S.SegmentConfig.Type.StringSegment { text = " | " }
      , S.SegmentConfig.Type.RowSegment
          { name = "system"
          , segments =
            [ S.SegmentConfig.Type.CpuSegment { name = "cpu" }
            , S.SegmentConfig.Type.StringSegment { text = " " }
            , S.SegmentConfig.Type.MemorySegment { name = "mem" }
            ]
          , style = Some
              S.StyleConfig::{
              , theme = Some "catppuccin"
              , themeForeground = Some "green"
              , themeBackground = Some "black"
              }
          , display = None S.DisplayConfig.Type
          }
      , S.SegmentConfig.Type.StringSegment { text = " | " }
      , S.SegmentConfig.Type.VolumeSegment { name = "vol" }
      , S.SegmentConfig.Type.StringSegment { text = " | " }
      , S.SegmentConfig.Type.MprisSegment { name = "song" }
      ]
    }

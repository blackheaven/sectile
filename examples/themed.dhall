let S = ../format.dhall

in  S.BarConfig::{
    , segments =
      [ S.SegmentNode::{
        , segment = S.Segment.Type.Time { name = "clock", format = "%H:%M:%S" }
        , style = Some S.StyleConfig::{ theme = Some "catppuccin", themeForeground = Some "blue", themeBackground = Some "black" }
        , row = Some 0
        }
      , S.SegmentNode::{
        , segment = S.Segment.Type.String { text = " " }
        , style = Some S.StyleConfig::{ theme = Some "catppuccin", themeForeground = Some "blue", themeBackground = Some "black" }
        , row = Some 0
        }
      , S.SegmentNode::{
        , segment = S.Segment.Type.Time { name = "date", format = "%Y-%m-%d" }
        , style = Some S.StyleConfig::{ theme = Some "catppuccin", themeForeground = Some "blue", themeBackground = Some "black" }
        , row = Some 0
        }
      , S.SegmentNode::{ segment = S.Segment.Type.String { text = " | " } }
      , S.SegmentNode::{
        , segment = S.Segment.Type.Cpu { name = "cpu" }
        , style = Some S.StyleConfig::{ theme = Some "catppuccin", themeForeground = Some "green", themeBackground = Some "black" }
        , row = Some 1
        }
      , S.SegmentNode::{
        , segment = S.Segment.Type.String { text = " " }
        , style = Some S.StyleConfig::{ theme = Some "catppuccin", themeForeground = Some "green", themeBackground = Some "black" }
        , row = Some 1
        }
      , S.SegmentNode::{
        , segment = S.Segment.Type.Memory { name = "mem" }
        , style = Some S.StyleConfig::{ theme = Some "catppuccin", themeForeground = Some "green", themeBackground = Some "black" }
        , row = Some 1
        }
      , S.SegmentNode::{ segment = S.Segment.Type.String { text = " | " } }
      , S.SegmentNode::{ segment = S.Segment.Type.Volume { name = "vol" } }
      , S.SegmentNode::{ segment = S.Segment.Type.String { text = " | " } }
      , S.SegmentNode::{ segment = S.Segment.Type.Mpris { name = "song" } }
      ]
    }

let S = ../format.dhall

in  S.BarConfig::{
    , segments =
      [ S.SegmentNode::{ segment = S.Segment.Type.Time { name = "clock", format = "%H:%M" } }
      , S.SegmentNode::{ segment = S.Segment.Type.String { text = " | " } }
      , S.SegmentNode::{ segment = S.Segment.Type.Shell { name = "host", command = "hostname" } }
      ]
    , separator = None Text
    }

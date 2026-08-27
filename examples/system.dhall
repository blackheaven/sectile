let S = ../format.dhall

in  S.BarConfig::{
    , segments =
      [ S.SegmentNode::{ segment = S.Segment.Type.Uptime { name = "uptime" } }
      , S.SegmentNode::{ segment = S.Segment.Type.Memory { name = "mem" } }
      , S.SegmentNode::{ segment = S.Segment.Type.Cpu { name = "cpu" } }
      , S.SegmentNode::{ segment = S.Segment.Type.Load { name = "load" } }
      , S.SegmentNode::{ segment = S.Segment.Type.Disk
          { name = "disk", mountPoint = "/" } }
      ]
    , separator = Some " | "
    }

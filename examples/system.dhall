let S = ../format.dhall

in  S.BarConfig::{
    , segments =
      [ S.SegmentNode::{ segment = S.SegmentConfig.Type.UptimeSegment { name = "uptime" } }
      , S.SegmentNode::{ segment = S.SegmentConfig.Type.MemorySegment { name = "mem" } }
      , S.SegmentNode::{ segment = S.SegmentConfig.Type.CpuSegment { name = "cpu" } }
      , S.SegmentNode::{ segment = S.SegmentConfig.Type.LoadSegment { name = "load" } }
      , S.SegmentNode::{ segment = S.SegmentConfig.Type.DiskSegment
          { name = "disk", mountPoint = "/" } }
      ]
    , separator = Some " | "
    }

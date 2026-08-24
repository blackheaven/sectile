let S = ../format.dhall

in  S.BarConfig::{
    , segments =
      [ S.SegmentConfig.Type.UptimeSegment { name = "uptime" }
      , S.SegmentConfig.Type.MemorySegment { name = "mem" }
      , S.SegmentConfig.Type.CpuSegment { name = "cpu" }
      , S.SegmentConfig.Type.LoadSegment { name = "load" }
      , S.SegmentConfig.Type.DiskSegment
          { name = "disk", mountPoint = "/" }
      ]
    , separator = Some " | "
    }

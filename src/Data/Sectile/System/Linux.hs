-- |
-- Module        : Data.Sectile.System.Linux
-- Copyright     : Gautier DI FOLCO
-- License       : ISC
--
-- Maintainer    : Gautier DI FOLCO <foss@difolco.dev>
-- Stability     : Stable
-- Portability   : Portable
--
module Data.Sectile.System.Linux
  ( uptime,
    memory,
    load,
    cpu,
    disk,
    networkUp,
    networkDown,
    battery,
    thermal,
    wifi,
  )
where

import Data.Sectile.System.Linux.Uptime
import Data.Sectile.System.Linux.Memory
import Data.Sectile.System.Linux.Load
import Data.Sectile.System.Linux.Cpu
import Data.Sectile.System.Linux.Disk
import Data.Sectile.System.Linux.Network
import Data.Sectile.System.Linux.Battery
import Data.Sectile.System.Linux.Thermal
import Data.Sectile.System.Linux.Wifi

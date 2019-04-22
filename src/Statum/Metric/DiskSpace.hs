{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Statum.Metric.DiskSpace
    ( usedPercent
    ) where


import Data.Function ((&))

import qualified System.DiskSpace as DiskSpace



usedPercent :: DiskSpace.DiskUsage -> Double
usedPercent DiskSpace.DiskUsage{..} =
    1 - (fromIntegral diskAvail / fromIntegral diskTotal)


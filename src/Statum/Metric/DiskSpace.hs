{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Statum.Metric.DiskSpace
    ( DiskUsage(..)
    , Error(..)
    , getDiskUsage
    , usedPercent
    ) where


import Data.Function ((&))

import qualified Control.Exception.Safe as Exception
import qualified Data.Bifunctor as Bifunctor
import qualified System.DiskSpace as DiskSpace


newtype Error = Error Exception.SomeException
    deriving (Show)


newtype DiskUsage = DiskUsage DiskSpace.DiskUsage
    deriving (Show)


getDiskUsage :: FilePath -> IO (Either Error DiskUsage)
getDiskUsage filepath =
    DiskSpace.getDiskUsage filepath
        & Exception.try
        & fmap (Bifunctor.first Error)
        & fmap (Bifunctor.second DiskUsage)



usedPercent :: DiskUsage -> Double
usedPercent (DiskUsage DiskSpace.DiskUsage{..}) =
    1 - (fromIntegral diskAvail / fromIntegral diskTotal)


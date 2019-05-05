module Statum.Task.DiskSpacePoller
    ( Config(..)
    , Metric(..)
    , DiskUsage(..)
    ) where


import qualified Data.Text as T
import qualified Statum.Api as Api


data Config = Config
    { filepath :: T.Text
    , interval :: Int
    , historyLength :: Int
    , metrics :: [Metric]
    }


data Metric
    = GetDiskUsage DiskUsage


data DiskUsage = DiskUsage
    { toWidget :: Double -> [Double] -> Api.Widget
    }

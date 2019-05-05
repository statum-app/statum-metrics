module Statum.Task.StatPoller
    ( Config(..)
    , Metric(..)
    , CpuUtilization(..)
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
    = GetCpuUtilization CpuUtilization


data CpuUtilization = CpuUtilization
    { toWidget :: ()
    }

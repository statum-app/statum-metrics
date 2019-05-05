module Statum.Task.MemInfoPoller
    ( Config(..)
    , Metric(..)
    , MemUsage(..)
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
    = GetMemUsage MemUsage


data MemUsage = MemUsage
    { toWidget :: Double -> [Double] -> Api.Widget
    }

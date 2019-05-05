module Statum.Task.InterfacePoller
    ( Config(..)
    , Metric(..)
    , NetworkRate(..)
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
    = GetNetworkTxRate NetworkRate
    | GetNetworkRxRate NetworkRate


data NetworkRate = NetworkRate
    { interfaceName :: T.Text
    , toWidget :: ()
    }

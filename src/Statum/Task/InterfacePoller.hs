module Statum.Task.InterfacePoller
    ( Config(..)
    , Metric(..)
    , NetworkRate(..)
    ) where


import qualified Data.Text as T
import qualified Dhall
import qualified GHC.Generics as GHC
import qualified Statum.Api as Api


data Config = Config
    { filepath :: T.Text
    , interval :: Dhall.Natural
    , historyLength :: Dhall.Natural
    , metrics :: [Metric]
    }
    deriving (GHC.Generic)

instance Dhall.Interpret Config


data Metric
    = GetNetworkTxRate NetworkRate
    | GetNetworkRxRate NetworkRate
    deriving (GHC.Generic)

instance Dhall.Interpret Metric


data NetworkRate = NetworkRate
    { interfaceName :: T.Text
    , toWidget :: T.Text
    }
    deriving (GHC.Generic)

instance Dhall.Interpret NetworkRate

module Statum.Task.MemInfoPoller
    ( Config(..)
    , Metric(..)
    , MemUsage(..)
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
    = GetMemUsage MemUsage
    deriving (GHC.Generic)

instance Dhall.Interpret Metric



data MemUsage = MemUsage
    { toWidget :: Double -> [Double] -> Api.Widget
    }
    deriving (GHC.Generic)

instance Dhall.Interpret MemUsage

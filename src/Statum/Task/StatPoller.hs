module Statum.Task.StatPoller
    ( Config(..)
    , Metric(..)
    , CpuUtilization(..)
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
    = GetCpuUtilization CpuUtilization
    deriving (GHC.Generic)

instance Dhall.Interpret Metric



data CpuUtilization = CpuUtilization
    { toWidget :: T.Text
    }
    deriving (GHC.Generic)

instance Dhall.Interpret CpuUtilization

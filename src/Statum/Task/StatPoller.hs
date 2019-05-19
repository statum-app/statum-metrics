{-# LANGUAGE RecordWildCards #-}

module Statum.Task.StatPoller
    ( Config(..)
    , Metric(..)
    , CpuUtilization(..)
    ) where


import Data.Function ((&))

import qualified Data.Text as T
import qualified Dhall.Core as Core
import qualified Dhall.Extra as Dhall
import qualified Dhall.Map as Map
import qualified GHC.Generics as GHC
import qualified Statum.Api as Api


data Config = Config
    { filepath :: FilePath
    , interval :: Dhall.Natural
    , historyLength :: Dhall.Natural
    , metrics :: [Metric]
    }
    deriving (GHC.Generic)

instance Dhall.Interpret Config



data Metric
    = GetCpuUtilization CpuUtilization
    deriving (GHC.Generic)

instance Dhall.Interpret Metric where
    autoWith _ = intepreter


intepreter :: Dhall.Type Metric
intepreter = Dhall.Type{..}
    where
        extract (Core.UnionLit type_ expr _) =
            case type_ of
                "GetCpuUtilization" ->
                    Dhall.extractAuto GetCpuUtilization expr

                _ ->
                    "Unsupported metric type: " <> type_
                        & T.unpack
                        & error

        extract _ =
            Nothing

        expected =
            Core.Union $
                Map.fromList
                    [ ("GetCpuUtilization", Dhall.expectedAuto GetCpuUtilization)
                    ]


data CpuUtilization = CpuUtilization
    { toWidget :: Double -> [Double] -> Api.Widget
    }
    deriving (GHC.Generic)

instance Dhall.Interpret CpuUtilization

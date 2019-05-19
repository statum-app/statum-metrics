{-# LANGUAGE RecordWildCards #-}

module Statum.Task.InterfacePoller
    ( Config(..)
    , Metric(..)
    , NetworkRate(..)
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
    = GetTransmitRate NetworkRate
    | GetReceiveRate NetworkRate
    deriving (GHC.Generic)

instance Dhall.Interpret Metric where
    autoWith _ = intepreter


intepreter :: Dhall.Type Metric
intepreter = Dhall.Type{..}
    where
        extract (Core.UnionLit type_ expr _) =
            case type_ of
                "GetTransmitRate" ->
                    Dhall.extractAuto GetTransmitRate expr

                "GetReceiveRate" ->
                    Dhall.extractAuto GetReceiveRate expr

                _ ->
                    "Unsupported metric type: " <> type_
                        & T.unpack
                        & error

        extract _ =
            Nothing

        expected =
            Core.Union $
                Map.fromList
                    [ ("GetTransmitRate", Dhall.expectedAuto GetTransmitRate)
                    , ("GetReceiveRate", Dhall.expectedAuto GetReceiveRate)
                    ]


data NetworkRate = NetworkRate
    { interfaceName :: T.Text
    , toWidget :: Double -> [Double] -> Api.Widget
    }
    deriving (GHC.Generic)

instance Dhall.Interpret NetworkRate

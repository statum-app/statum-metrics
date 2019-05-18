{-# LANGUAGE RecordWildCards #-}

module Statum.Task.DiskSpacePoller
    ( Config(..)
    , Metric(..)
    , DiskUsage(..)
    ) where


import Data.Function ((&))

import qualified Data.Text as T
import qualified Dhall
import qualified Dhall.Core as Core
import qualified Dhall.Map as Map
import qualified Dhall.Parser as Parser
import qualified Dhall.TypeCheck as TypeCheck
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
    = GetDiskUsage DiskUsage
    | Void NoOp
    deriving (GHC.Generic)


instance Dhall.Interpret Metric where
    autoWith _ = taskInterpreter


taskInterpreter :: Dhall.Type Metric
taskInterpreter = Dhall.Type{..}
    where
        extract (Core.UnionLit type_ expr _) =
            case type_ of
                "GetDiskUsage" ->
                    extractTask GetDiskUsage expr

                "Void" ->
                    extractTask Void expr

                _ ->
                    "Unsupported metric type: " <> type_
                        & T.unpack
                        & error

        extract _ =
            Nothing

        expected =
            Core.Union $
                Map.fromList
                    [ ("GetDiskUsage", expectedTask GetDiskUsage)
                    , ("Void", expectedTask Void)
                    ]


extractTask :: Dhall.Interpret a => (a -> Metric) -> Core.Expr Parser.Src TypeCheck.X -> Maybe Metric
extractTask toTask expr =
    Dhall.auto
        & fmap toTask
        & \value -> Dhall.extract value expr


expectedTask :: Dhall.Interpret a => (a -> Metric) -> Core.Expr Parser.Src TypeCheck.X
expectedTask toTask =
    Dhall.auto
        & fmap toTask
        & Dhall.expected



data DiskUsage = DiskUsage
    { toWidget :: Double -> [Double] -> Api.Widget
    }
    deriving (GHC.Generic)

instance Dhall.Interpret DiskUsage



data NoOp = NoOp
    { noOp :: Bool
    }
    deriving (GHC.Generic)

instance Dhall.Interpret NoOp

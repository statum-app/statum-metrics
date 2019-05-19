{-# LANGUAGE RecordWildCards #-}

module Statum.Task
    ( Task(..)
    ) where


import Data.Function ((&))

import qualified Data.Text as T
import qualified Dhall.Core as Core
import qualified Dhall.Extra as Dhall
import qualified Dhall.Map as Map
import qualified GHC.Generics as GHC
import qualified Statum.Task.DiskSpacePoller as DiskSpacePoller
import qualified Statum.Task.InterfacePoller as InterfacePoller
import qualified Statum.Task.MemInfoPoller as MemInfoPoller
import qualified Statum.Task.StatPoller as StatPoller


data Task
    = MemInfoPoller MemInfoPoller.Config
    | DiskSpacePoller DiskSpacePoller.Config
    | InterfacePoller InterfacePoller.Config
    | StatPoller StatPoller.Config
    deriving (GHC.Generic)


instance Dhall.Interpret Task where
    autoWith _ = taskInterpreter


taskInterpreter :: Dhall.Type Task
taskInterpreter = Dhall.Type{..}
    where
        extract (Core.UnionLit type_ expr _) =
            case type_ of
                "MemInfoPoller" ->
                    Dhall.extractAuto MemInfoPoller expr

                "DiskSpacePoller" ->
                    Dhall.extractAuto DiskSpacePoller expr

                "InterfacePoller" ->
                    Dhall.extractAuto InterfacePoller expr

                "StatPoller" ->
                    Dhall.extractAuto StatPoller expr

                _ ->
                    "Unsupported task type: " <> type_
                        & T.unpack
                        & error

        extract _ =
            Nothing

        expected =
            Core.Union $
                Map.fromList
                    [ ("MemInfoPoller", Dhall.expectedAuto MemInfoPoller)
                    , ("DiskSpacePoller", Dhall.expectedAuto DiskSpacePoller)
                    , ("InterfacePoller", Dhall.expectedAuto InterfacePoller)
                    , ("StatPoller", Dhall.expectedAuto StatPoller)
                    ]

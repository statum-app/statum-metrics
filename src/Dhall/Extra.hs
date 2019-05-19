module Dhall.Extra
    ( extractAuto
    , expectedAuto

    , module Dhall
    ) where


import Data.Function ((&))

import Dhall
import qualified Dhall.Core as Core
import qualified Dhall.Parser as Parser
import qualified Dhall.TypeCheck as TypeCheck



extractAuto :: Dhall.Interpret a => (a -> b) -> Core.Expr Parser.Src TypeCheck.X -> Maybe b
extractAuto mapper expr =
    Dhall.auto
        & fmap mapper
        & \value -> Dhall.extract value expr


expectedAuto :: Dhall.Interpret a => (a -> b) -> Core.Expr Parser.Src TypeCheck.X
expectedAuto mapper =
    Dhall.auto
        & fmap mapper
        & Dhall.expected

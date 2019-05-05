{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}


module Statum.Widget.Number
    ( Number(..)
    ) where



import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import qualified Dhall
import qualified GHC.Generics as GHC




data Number = Number
    { title :: T.Text
    , current :: Double
    , previous :: Double
    , moreInfo :: T.Text
    }
    deriving (Show, GHC.Generic)


instance Aeson.FromJSON Number
instance Aeson.ToJSON Number
instance Dhall.Interpret Number

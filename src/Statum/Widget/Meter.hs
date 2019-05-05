{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Statum.Widget.Meter
    ( Meter(..)
    ) where



import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import qualified GHC.Generics as GHC




data Meter = Meter
    { title :: T.Text
    , value :: Double
    }
    deriving (Show, GHC.Generic)


instance Aeson.FromJSON Meter
instance Aeson.ToJSON Meter

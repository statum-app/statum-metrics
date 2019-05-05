{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Statum.Metric
    ( Metric(..)
    , cpuUtilization
    , memUsage
    , diskUsage
    , networkTxRate
    , networkRxRate
    ) where



import Data.Function ((&))

import qualified Data.Text as T


data Metric
    = CpuUtilization (Value Double)
    | NetworkTxRate (Value NetworkRate)
    | NetworkRxRate (Value NetworkRate)
    | MemUsage (Value Double)
    | DiskUsage (Value Double)
    deriving (Show)



data Value a = Value
    { current :: a
    , previous :: [a]
    }
    deriving (Show)


data NetworkRate = NetworkRate
    { interfaceName :: T.Text
    , bytesPerSecond :: Int
    }
    deriving (Show)


cpuUtilization :: Double -> [Double] -> Metric
cpuUtilization current previous =
    Value current previous
        & CpuUtilization


memUsage :: Double -> [Double] -> Metric
memUsage current previous =
    Value current previous
        & MemUsage


diskUsage :: Double -> [Double] -> Metric
diskUsage current previous =
    Value current previous
        & DiskUsage


networkTxRate :: T.Text -> Int -> [Int] -> Metric
networkTxRate ifaceName current previous =
    let
        toNetworkRate =
            NetworkRate ifaceName
    in
    Value (toNetworkRate current) (map toNetworkRate previous)
        & NetworkTxRate


networkRxRate :: T.Text -> Int -> [Int] -> Metric
networkRxRate ifaceName current previous =
    let
        toNetworkRate =
            NetworkRate ifaceName
    in
    Value (toNetworkRate current) (map toNetworkRate previous)
        & NetworkRxRate

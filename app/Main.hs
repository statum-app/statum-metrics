{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Control.Monad as Monad
import qualified Data.Text.IO as TextIO
import qualified Statum.Proc.Net.Dev as Dev
import qualified Statum.Proc.Stat as Stat

import Data.Function

main :: IO ()
main = do
    --input <- TextIO.readFile "stat.txt"
    --let eitherStat = Stat.parse input
    --print $ cpuPercent eitherStat

    input <- TextIO.readFile "dev.txt"
    print $ Dev.parse input
    pure ()
    --
    --print $ Stat.parse "cpu  231992 0 121126 275336733 4409 0 876 0 0 0"
    --

cpuPercent :: Either String Stat.Stat -> Either String Double
cpuPercent eitherStat = do
    Stat.Stat{..} <- eitherStat
    pure $ Stat.cpuUtilization statCpuTotal

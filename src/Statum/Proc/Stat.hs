{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Statum.Proc.Stat
    ( parse
    ) where

import Data.Attoparsec.Text ((<?>))

import qualified Data.Attoparsec.Text as Parser
import Data.Function ((&))
import qualified Data.Text as T


-- https://github.com/torvalds/linux/blob/master/Documentation/filesystems/proc.txt#L1323

data Stat = Stat
    { cpuTotal :: CpuStat
    , cpuCores :: [CpuCoreStat]
    , interupts :: [Integer]
    , contextSwitches :: Integer
    , bootTime :: Integer
    , processes :: Integer
    , processesRunning :: Integer
    , processBlocked :: Integer
    , softIrq :: [Integer]
    }
    deriving (Show)


parse :: T.Text -> Either String Stat
parse input = do
    Parser.parseOnly statParser input


statParser :: Parser.Parser Stat
statParser = do
    cpuTotal <- cpuTotalParser
        <* endOfLineParser "after cpu"
    cpuCores <- cpuCoreParser
        <* endOfLineParser "after cpuN"
        & Parser.many'
    interupts <- interuptParser
        <* endOfLineParser "after intr"
    contextSwitches <- contextSwitchParser
        <* endOfLineParser "after ctxt"
    bootTime <- bootTimeParser
        <* endOfLineParser "after btime"
    processes <- processesParser
        <* endOfLineParser "after processes"
    processesRunning <- processesRunningParser
        <* endOfLineParser "after procs_running"
    processBlocked <- processesBlockedParser
        <* endOfLineParser "after procs_blocked"
    softIrq <- softIrqParser
        <* endOfLineParser "after softirq"
    pure $ Stat
        { cpuTotal
        , cpuCores
        , interupts
        , contextSwitches
        , bootTime
        , processes
        , processesRunning
        , processBlocked
        , softIrq
        }


data CpuCoreStat
    = CpuCoreStat Int CpuStat
    deriving (Show)



data CpuStat = CpuStat
    { cpuStatUser :: Integer
    , cpuStatNice :: Integer
    , cpuStatSystem :: Integer
    , cpuStatIdle :: Integer
    , cpuStatIowait :: Integer
    , cpuStatIrq :: Integer
    , cpuStatSoftIrq :: Integer
    , cpuStatSteal :: Integer
    , cpuStatGuest :: Integer
    , cpuStatGuestNice :: Integer
    }
    deriving (Show)



cpuTotalParser :: Parser.Parser CpuStat
cpuTotalParser = do
    textParser "cpu"
    cpuStatParser


cpuCoreParser :: Parser.Parser CpuCoreStat
cpuCoreParser = do
    textParser "cpu"
    cpuN <- Parser.decimal
    cpuStatParser
        & fmap (CpuCoreStat cpuN)


cpuStatParser :: Parser.Parser CpuStat
cpuStatParser = do
    cpuStatUser <- decimalParser
    cpuStatNice <- decimalParser
    cpuStatSystem <- decimalParser
    cpuStatIdle <- decimalParser
    cpuStatIowait <- decimalParser
    cpuStatIrq <- decimalParser
    cpuStatSoftIrq <- decimalParser
    cpuStatSteal <- decimalParser
    cpuStatGuest <- decimalParser
    cpuStatGuestNice <- decimalParser
    pure $ CpuStat
        { cpuStatUser
        , cpuStatNice
        , cpuStatSystem
        , cpuStatIdle
        , cpuStatIowait
        , cpuStatIrq
        , cpuStatSoftIrq
        , cpuStatSteal
        , cpuStatGuest
        , cpuStatGuestNice
        }


interuptParser :: Parser.Parser [Integer]
interuptParser = do
    textParser "intr"
    Parser.many1 decimalParser


contextSwitchParser :: Parser.Parser Integer
contextSwitchParser = do
    textParser "ctxt"
    decimalParser


bootTimeParser :: Parser.Parser Integer
bootTimeParser = do
    textParser "btime"
    decimalParser


processesParser :: Parser.Parser Integer
processesParser = do
    textParser "processes"
    decimalParser


processesRunningParser :: Parser.Parser Integer
processesRunningParser = do
    textParser "procs_running"
    decimalParser


processesBlockedParser :: Parser.Parser Integer
processesBlockedParser = do
    textParser "procs_blocked"
    decimalParser


softIrqParser :: Parser.Parser [Integer]
softIrqParser = do
    textParser "softirq"
    Parser.many1 decimalParser


decimalParser :: Parser.Parser Integer
decimalParser = do
    Parser.skipSpace
    Parser.decimal


textParser :: T.Text -> Parser.Parser T.Text
textParser text =
    Parser.string text
        <?> failureMsg text


endOfLineParser :: T.Text -> Parser.Parser ()
endOfLineParser msg =
    Parser.endOfLine
        <?> failureMsg ("endOfLine " <> msg)


failureMsg :: T.Text -> String
failureMsg name =
    [ "Failed to parse "
    , name
    ]
    & mconcat
    & T.unpack

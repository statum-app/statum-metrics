{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Statum.Proc.Stat
    ( Stat(..)
    , CpuStat(..)
    , parse
    , cpuUtilization
    ) where

import Data.Attoparsec.Text ((<?>))

import qualified Data.Attoparsec.Text as Parser
import Data.Function ((&))
import qualified Data.Text as T


-- https://github.com/torvalds/linux/blob/master/Documentation/filesystems/proc.txt#L1323

data Stat = Stat
    { statCpuTotal :: CpuStat
    , statCpuCores :: [CpuCoreStat]
    , statInterupts :: [Int]
    , statContextSwitches :: Int
    , statBootTime :: Int
    , statProcesses :: Int
    , statProcessesRunning :: Int
    , statProcessBlocked :: Int
    , statSoftIrq :: [Int]
    }
    deriving (Show)


data CpuCoreStat
    = CpuCoreStat Int CpuStat
    deriving (Show)


data CpuStat = CpuStat
    { cpuStatUser :: Int
    , cpuStatNice :: Int
    , cpuStatSystem :: Int
    , cpuStatIdle :: Int
    , cpuStatIowait :: Int
    , cpuStatIrq :: Int
    , cpuStatSoftIrq :: Int
    , cpuStatSteal :: Int
    , cpuStatGuest :: Int
    , cpuStatGuestNice :: Int
    }
    deriving (Show)


cpuUtilization :: CpuStat -> Double
cpuUtilization CpuStat{..} =
    [ cpuStatUser
    , cpuStatNice
    , cpuStatSystem
    , cpuStatIdle
    , cpuStatIowait
    , cpuStatIrq
    , cpuStatSoftIrq
    , cpuStatSteal
    , cpuStatGuest
    , cpuStatGuestNice
    ]
    & sum
    & fromIntegral
    & (/) (fromIntegral cpuStatIdle)
    & (-) 1
    & (*) 100



parse :: T.Text -> Either String Stat
parse input = do
    Parser.parseOnly statParser input


statParser :: Parser.Parser Stat
statParser = do
    statCpuTotal <- cpuTotalParser
        <* endOfLineParser "after cpu"
    statCpuCores <- cpuCoreParser
        <* endOfLineParser "after cpuN"
        & Parser.many'
    statInterupts <- interuptParser
        <* endOfLineParser "after intr"
    statContextSwitches <- contextSwitchParser
        <* endOfLineParser "after ctxt"
    statBootTime <- bootTimeParser
        <* endOfLineParser "after btime"
    statProcesses <- processesParser
        <* endOfLineParser "after processes"
    statProcessesRunning <- processesRunningParser
        <* endOfLineParser "after procs_running"
    statProcessBlocked <- processesBlockedParser
        <* endOfLineParser "after procs_blocked"
    statSoftIrq <- softIrqParser
        <* endOfLineParser "after softirq"
    pure $ Stat
        { statCpuTotal
        , statCpuCores
        , statInterupts
        , statContextSwitches
        , statBootTime
        , statProcesses
        , statProcessesRunning
        , statProcessBlocked
        , statSoftIrq
        }



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


interuptParser :: Parser.Parser [Int]
interuptParser = do
    textParser "intr"
    Parser.many1 decimalParser


contextSwitchParser :: Parser.Parser Int
contextSwitchParser = do
    textParser "ctxt"
    decimalParser


bootTimeParser :: Parser.Parser Int
bootTimeParser = do
    textParser "btime"
    decimalParser


processesParser :: Parser.Parser Int
processesParser = do
    textParser "processes"
    decimalParser


processesRunningParser :: Parser.Parser Int
processesRunningParser = do
    textParser "procs_running"
    decimalParser


processesBlockedParser :: Parser.Parser Int
processesBlockedParser = do
    textParser "procs_blocked"
    decimalParser


softIrqParser :: Parser.Parser [Int]
softIrqParser = do
    textParser "softirq"
    Parser.many1 decimalParser


decimalParser :: Parser.Parser Int
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

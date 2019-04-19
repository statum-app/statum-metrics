{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Function ((&))

import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TChan as TChan
import qualified Control.Monad as Monad
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Either.Combinators as Combinators
import qualified Data.Text as T
import qualified Data.Text.IO as TextIO
import qualified Data.Time.Clock as Clock
import qualified Safe
import qualified Statum.Proc.Net.Dev as Dev
import qualified Statum.Proc.Stat as Stat
import qualified Statum.Reader as Reader




data InputError
    = ReadDevError Reader.Error
    | ParseDevError String
    | ReadStatError Reader.Error
    | ParseStatError String
    deriving (Show)


data Msg
    = DevMsg (Reader.Msg [Dev.InterfaceSnapshot])
    | StatMsg (Reader.Msg Stat.Stat)
    deriving (Show)



devReaderConfig :: TChan.TChan (Either InputError Msg) -> Reader.Config Msg InputError [Dev.InterfaceSnapshot]
devReaderConfig chan =
    Reader.Config
        { filepath = "/proc/net/dev"
        , mapper = parseDev
        , toMsg = DevMsg
        , chan = chan
        , interval = Reader.Second 5
        , historyLength = 1
        }


statReaderConfig :: TChan.TChan (Either InputError Msg) -> Reader.Config Msg InputError Stat.Stat
statReaderConfig chan =
    Reader.Config
        { filepath = "/proc/stat"
        , mapper = parseStat
        , toMsg = StatMsg
        , chan = chan
        , interval = Reader.Second 5
        , historyLength = 0
        }


parseDev :: Either Reader.Error Reader.Result -> Either InputError [Dev.InterfaceSnapshot]
parseDev eitherResult = do
    Reader.Result{..} <- eitherResult
        & Bifunctor.first ReadDevError
    interfaces <- Dev.parse fileContents
        & Bifunctor.first ParseDevError
    interfaces
        & map (Dev.InterfaceSnapshot timestamp)
        & pure


parseStat :: Either Reader.Error Reader.Result -> Either InputError Stat.Stat
parseStat eitherResult = do
    Reader.Result{..} <- eitherResult
        & Bifunctor.first ReadStatError
    Stat.parse fileContents
        & Bifunctor.first ParseStatError


main :: IO ()
main = do
    broadcastChan <- TChan.newBroadcastTChan
        & STM.atomically
    broadcastChan
        & devReaderConfig
        & Reader.forkReader
    broadcastChan
        & statReaderConfig
        & Reader.forkReader
    chan <- TChan.dupTChan broadcastChan
        & STM.atomically
    Monad.forever $ do
        eitherMsg <- TChan.readTChan chan
            & STM.atomically
        case handleEitherMsg eitherMsg of
            Left reason ->
                handleError reason

            Right metrics ->
                mapM_ print metrics


data Reason
    = Input InputError
    | MissingPrevious
    | InterfaceNotFound T.Text
    | InvalidRate T.Text
    deriving (Show)


data Metric
    = CpuUtilization Double
    | NetworkTxRate NetworkRateMetric
    | NetworkRxRate NetworkRateMetric
    deriving (Show)


data NetworkRateMetric = NetworkRateMetric
    { interfaceName :: T.Text
    , bytesPerSecond :: Double
    }
    deriving (Show)


handleEitherMsg :: Either InputError Msg -> Either Reason [Metric]
handleEitherMsg eitherMsg = do
    msg <- eitherMsg
        & Bifunctor.first Input
    handleMsg msg


handleError :: Reason -> IO ()
handleError reason =
    print ("reason", reason)


handleMsg :: Msg -> Either Reason [Metric]
handleMsg msg =
    case msg of
        DevMsg Reader.Msg{..} ->
            handleDevMsg "eno1" current previous

        StatMsg Reader.Msg{..} ->
            handleStatMsg current


handleDevMsg :: T.Text -> [Dev.InterfaceSnapshot] -> [[Dev.InterfaceSnapshot]] -> Either Reason [Metric]
handleDevMsg ifaceName current previous = do
    currentSnapshot <- Dev.findSnapshot current ifaceName
        & Combinators.maybeToRight (InterfaceNotFound ifaceName)
    prevSnapshots <- Safe.headMay previous
        & Combinators.maybeToRight MissingPrevious
    prevSnapshot <- Dev.findSnapshot prevSnapshots ifaceName
        & Combinators.maybeToRight (InterfaceNotFound ifaceName)
    txRate <- Dev.txRate currentSnapshot prevSnapshot
        & Combinators.maybeToRight (InvalidRate ifaceName)
    rxRate <- Dev.rxRate currentSnapshot prevSnapshot
        & Combinators.maybeToRight (InvalidRate ifaceName)
    pure
        [ txRate
            & NetworkRateMetric ifaceName
            & NetworkTxRate
        , rxRate
            & NetworkRateMetric ifaceName
            & NetworkRxRate
        ]


handleStatMsg :: Stat.Stat -> Either Reason [Metric]
handleStatMsg Stat.Stat{..} =
    Stat.cpuUtilization statCpuTotal
        & CpuUtilization
        & pure
        & pure

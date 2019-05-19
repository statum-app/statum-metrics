{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Function ((&))
import Statum.Metric (Metric)


import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TChan as TChan
import qualified Control.Monad as Monad
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Either.Combinators as Combinators
import qualified Data.Text as T
import qualified Data.Text.IO as TextIO
import qualified Data.Time.Clock as Clock
import qualified Dhall
import qualified GHC.Generics as GHC
import qualified Network.HTTP.Client.TLS as TLSClient
import qualified Safe
import qualified Statum.Api as Api
import qualified Statum.Interval as Interval
import qualified Statum.Metric as Metric
import qualified Statum.Metric.DiskSpace as DiskSpace
import qualified Statum.Poller as Poller
import qualified Statum.Proc.MemInfo as MemInfo
import qualified Statum.Proc.Net.Dev as Dev
import qualified Statum.Proc.Stat as Stat
import qualified Statum.Reader as Reader
import qualified Statum.Task as Task
import qualified Statum.Task.DiskSpacePoller as DiskSpacePoller
import qualified Statum.Task.MemInfoPoller as MemInfoPoller



data Config = Config
    { tasks :: [Task.Task]
    }
    deriving (GHC.Generic)

instance Dhall.Interpret Config



getConfig :: IO Config
getConfig =
    Dhall.detailed (Dhall.input Dhall.auto "./config.dhall")


main :: IO ()
main = do
    Config{..} <- getConfig
    manager <- TLSClient.newTlsManager
    broadcastChan <- TChan.newBroadcastTChan
        & STM.atomically
    mapM_ (startTask broadcastChan) tasks
    --interfacePollerConfig "/proc/net/dev" broadcastChan
    --    & Poller.poller
    --    & Interval.startWithState (Interval.Second 5) []
    --statPollerConfig "/proc/stat" broadcastChan
    --    & Poller.poller
    --    & Interval.startWithState (Interval.Second 5) []
    chan <- TChan.dupTChan broadcastChan
        & STM.atomically
    Monad.forever $ do
        -- TODO: channel may build up if api is slow
        eitherMsg <- TChan.readTChan chan
            & STM.atomically
        handleEitherMsg eitherMsg
            & mapM_ handleMetric


handleMetric :: Either Reason Metric -> IO ()
handleMetric res =
    case res of
        Left reason ->
            handleError reason

        Right metric ->
            print metric



startTask :: TChan.TChan (Either InputError Msg) -> Task.Task -> IO ()
startTask chan task =
    case task of
        Task.MemInfoPoller config@MemInfoPoller.Config{..} ->
            memInfoPollerConfig config chan
                & Poller.poller
                & Interval.startWithState (Interval.Second interval) []

        Task.DiskSpacePoller config@DiskSpacePoller.Config{..} ->
            diskSpacePollerConfig config chan
                & Poller.poller
                & Interval.startWithState (Interval.Second interval) []


data InputError
    = ReadDevError Reader.Error
    | ParseDevError String
    | ReadStatError Reader.Error
    | ParseStatError String
    | ReadMemInfoError Reader.Error
    | ParseMemInfoError String
    | GetDiskUsageError DiskSpace.Error
    deriving (Show)


data Msg
    = InterfaceDevMsg (Poller.Msg [Dev.InterfaceSnapshot])
    | StatMsg (Poller.Msg Stat.StatSnapshot)
    | MemInfoMsg (Poller.Msg MemInfo.MemInfo)
    | DiskSpaceMsg [DiskSpacePoller.Metric] (Poller.Msg DiskSpace.DiskUsage)





--interfacePollerConfig :: FilePath -> TChan.TChan (Either InputError Msg) -> Poller.Config Msg InputError [Dev.InterfaceSnapshot]
--interfacePollerConfig filepath chan =
--    Poller.Config
--        { action = Reader.reader filepath
--            & fmap parseDev
--        , toMsg = InterfaceDevMsg
--        , chan = chan
--        , historyLength = 1
--        }
--
--
--statPollerConfig :: FilePath -> TChan.TChan (Either InputError Msg) -> Poller.Config Msg InputError Stat.StatSnapshot
--statPollerConfig filepath chan =
--    Poller.Config
--        { action = Reader.reader filepath
--            & fmap parseStat
--        , toMsg = StatMsg
--        , chan = chan
--        , historyLength = 1
--        }


memInfoPollerConfig :: MemInfoPoller.Config -> TChan.TChan (Either InputError Msg) -> Poller.Config Msg InputError MemInfo.MemInfo
memInfoPollerConfig MemInfoPoller.Config{..} chan =
    Poller.Config
        { action = Reader.reader filepath
            & fmap parseMemInfo
        , toMsg = MemInfoMsg
        , chan = chan
        , historyLength = historyLength
        }


diskSpacePollerConfig :: DiskSpacePoller.Config -> TChan.TChan (Either InputError Msg) -> Poller.Config Msg InputError DiskSpace.DiskUsage
diskSpacePollerConfig DiskSpacePoller.Config{..} chan =
    Poller.Config
        { action = DiskSpace.getDiskUsage filepath
            & fmap (Bifunctor.first GetDiskUsageError)
        , toMsg = DiskSpaceMsg metrics
        , chan = chan
        , historyLength = historyLength
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


parseStat :: Either Reader.Error Reader.Result -> Either InputError Stat.StatSnapshot
parseStat eitherResult = do
    Reader.Result{..} <- eitherResult
        & Bifunctor.first ReadStatError
    Stat.parse fileContents
        & Bifunctor.first ParseStatError
        & fmap (Stat.StatSnapshot timestamp)


parseMemInfo :: Either Reader.Error Reader.Result -> Either InputError MemInfo.MemInfo
parseMemInfo eitherResult = do
    Reader.Result{..} <- eitherResult
        & Bifunctor.first ReadMemInfoError
    MemInfo.parse fileContents
        & Bifunctor.first ParseMemInfoError


data Reason
    = Input InputError
    | MissingPreviousInterface
    | InterfaceNotFound T.Text
    | InvalidRate T.Text
    | MissingPreviousStat
    | InvalidCpuUtilisation
    deriving (Show)



handleEitherMsg :: Either InputError Msg -> [Either Reason Metric]
handleEitherMsg eitherMsg = do
    case eitherMsg of
        Left err ->
            [Left (Input err)]

        Right msg ->
            handleMsg msg


handleError :: Reason -> IO ()
handleError reason =
    print ("reason", reason)


handleMsg :: Msg -> [Either Reason Metric]
handleMsg msg =
    case msg of
        --InterfaceDevMsg Poller.Msg{..} ->
        --    handleDevMsg "eno1" current previous

        --StatMsg Poller.Msg{..} ->
        --    handleStatMsg current previous

        MemInfoMsg Poller.Msg{..} ->
            [handleMemInfoMsg current previous]

        DiskSpaceMsg metrics Poller.Msg{..} ->
            map (getDiskSpaceMetric current previous) metrics


--handleDevMsg :: T.Text -> [Dev.InterfaceSnapshot] -> [[Dev.InterfaceSnapshot]] -> Either Reason Metric
--handleDevMsg ifaceName current previous = do
--    currentSnapshot <- Dev.findSnapshot current ifaceName
--        & Combinators.maybeToRight (InterfaceNotFound ifaceName)
--    prevSnapshots <- Safe.headMay previous
--        & Combinators.maybeToRight MissingPreviousInterface
--    prevSnapshot <- Dev.findSnapshot prevSnapshots ifaceName
--        & Combinators.maybeToRight (InterfaceNotFound ifaceName)
--    txRate <- Dev.txRate currentSnapshot prevSnapshot
--        & Combinators.maybeToRight (InvalidRate ifaceName)
--    rxRate <- Dev.rxRate currentSnapshot prevSnapshot
--        & Combinators.maybeToRight (InvalidRate ifaceName)
--    -- TODO: send previous values
--    pure
--        [ Metric.networkTxRate ifaceName txRate []
--        , Metric.networkRxRate ifaceName rxRate []
--        ]
--
--
--handleStatMsg :: Stat.StatSnapshot -> [Stat.StatSnapshot] -> Either Reason Metric
--handleStatMsg current previous = do
--    prev <- Safe.headMay previous
--        & Combinators.maybeToRight MissingPreviousStat
--    utilisation <- Stat.cpuUtilisation current prev
--        & Combinators.maybeToRight (InvalidCpuUtilisation)
--    -- TODO: send previous values
--    Metric.cpuUtilization utilisation []
--        & pure
--        & pure
--
--
handleMemInfoMsg :: MemInfo.MemInfo -> [MemInfo.MemInfo] -> Either Reason Metric
handleMemInfoMsg current previous =
    Metric.memUsage (MemInfo.usedPercent current) (map MemInfo.usedPercent previous)
        & pure


getDiskSpaceMetric :: DiskSpace.DiskUsage -> [DiskSpace.DiskUsage] -> DiskSpacePoller.Metric -> Either Reason Metric
getDiskSpaceMetric current previous metric =
    case metric of
        DiskSpacePoller.GetDiskUsage config ->
            Metric.diskUsage (DiskSpace.usedPercent current) (map DiskSpace.usedPercent previous)
                & pure

        DiskSpacePoller.Void _ ->
            error "TODO: get rid of Void"

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
import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as TextIO
import qualified Data.Time.Clock as Clock
import qualified Dhall
import qualified GHC.Generics as GHC
import qualified Network.HTTP.Client as Client
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
import qualified Statum.Task.InterfacePoller as InterfacePoller
import qualified Statum.Task.MemInfoPoller as MemInfoPoller
import qualified Statum.Task.StatPoller as StatPoller



data Config = Config
    { tasks :: [Task.Task]
    , apiBaseUrl :: String
    }
    deriving (GHC.Generic)

instance Dhall.Interpret Config



getConfig :: IO Config
getConfig =
    Dhall.detailed (Dhall.input Dhall.auto "./config.dhall")


main :: IO ()
main = do
    config@Config{..} <- getConfig
    manager <- TLSClient.newTlsManager
    broadcastChan <- TChan.newBroadcastTChan
        & STM.atomically
    mapM_ (startTask broadcastChan) tasks
    chan <- TChan.dupTChan broadcastChan
        & STM.atomically
    Monad.forever $ do
        -- TODO: channel may build up if api is slow
        eitherMsg <- TChan.readTChan chan
            & STM.atomically
        handleEitherMsg eitherMsg
            & mapM_ (handleWidget config manager)


handleWidget :: Config -> Client.Manager -> Either Reason Api.Widget -> IO ()
handleWidget Config{..} manager res =
    case res of
        Left reason ->
            handleError reason

        Right widget -> do
            print widget
            response <- Api.prepareRequest apiBaseUrl widget
                & Api.sendRequest manager
            print response



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

        Task.InterfacePoller config@InterfacePoller.Config{..} ->
            interfacePollerConfig config chan
                & Poller.poller
                & Interval.startWithState (Interval.Second interval) []

        Task.StatPoller config@StatPoller.Config{..} ->
            statPollerConfig config chan
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
    = MemInfoMsg [MemInfoPoller.Metric] (Poller.Msg MemInfo.MemInfo)
    | DiskSpaceMsg [DiskSpacePoller.Metric] (Poller.Msg DiskSpace.DiskUsage)
    | InterfaceDevMsg [InterfacePoller.Metric](Poller.Msg [Dev.InterfaceSnapshot])
    | StatMsg [StatPoller.Metric] (Poller.Msg Stat.StatSnapshot)




memInfoPollerConfig :: MemInfoPoller.Config -> TChan.TChan (Either InputError Msg) -> Poller.Config Msg InputError MemInfo.MemInfo
memInfoPollerConfig MemInfoPoller.Config{..} chan =
    Poller.Config
        { action = Reader.reader filepath
            & fmap parseMemInfo
        , toMsg = MemInfoMsg metrics
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


interfacePollerConfig :: InterfacePoller.Config -> TChan.TChan (Either InputError Msg) -> Poller.Config Msg InputError [Dev.InterfaceSnapshot]
interfacePollerConfig InterfacePoller.Config{..} chan =
    Poller.Config
        { action = Reader.reader filepath
            & fmap parseDev
        , toMsg = InterfaceDevMsg metrics
        , chan = chan
        , historyLength = historyLength
        }


statPollerConfig :: StatPoller.Config -> TChan.TChan (Either InputError Msg) -> Poller.Config Msg InputError Stat.StatSnapshot
statPollerConfig StatPoller.Config{..} chan =
    Poller.Config
        { action = Reader.reader filepath
            & fmap parseStat
        , toMsg = StatMsg metrics
        , chan = chan
        , historyLength = 1
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



handleEitherMsg :: Either InputError Msg -> [Either Reason Api.Widget]
handleEitherMsg eitherMsg = do
    case eitherMsg of
        Left err ->
            [Left (Input err)]

        Right msg ->
            handleMsg msg


handleError :: Reason -> IO ()
handleError reason =
    print ("reason", reason)


handleMsg :: Msg -> [Either Reason Api.Widget]
handleMsg msg =
    case msg of
        MemInfoMsg metrics Poller.Msg{..} ->
            map (getMemInfoMetric current previous) metrics

        DiskSpaceMsg metrics Poller.Msg{..} ->
            map (getDiskSpaceMetric current previous) metrics

        InterfaceDevMsg metrics Poller.Msg{..} ->
            map (getInterfaceMetric current previous) metrics

        StatMsg metrics Poller.Msg{..} ->
            map (getStatMetric current previous) metrics



getMemInfoMetric :: MemInfo.MemInfo -> [MemInfo.MemInfo] -> MemInfoPoller.Metric -> Either Reason Api.Widget
getMemInfoMetric current previous metric =
    case metric of
        MemInfoPoller.GetMemUsage MemInfoPoller.MemUsage{..} ->
            toWidget (MemInfo.usedPercent current) (map MemInfo.usedPercent previous)
                & pure


getDiskSpaceMetric :: DiskSpace.DiskUsage -> [DiskSpace.DiskUsage] -> DiskSpacePoller.Metric -> Either Reason Api.Widget
getDiskSpaceMetric current previous metric =
    case metric of
        DiskSpacePoller.GetDiskUsage DiskSpacePoller.DiskUsage{..} ->
            toWidget (DiskSpace.usedPercent current) (map DiskSpace.usedPercent previous)
                & pure


-- TODO: refactor duplication
getInterfaceMetric :: [Dev.InterfaceSnapshot] -> [[Dev.InterfaceSnapshot]] -> InterfacePoller.Metric -> Either Reason Api.Widget
getInterfaceMetric current previous metric =
    case metric of
        InterfacePoller.GetTransmitRate InterfacePoller.NetworkRate{..} -> do
            (currentSnapshot, prevSnapshots) <- interfaceSnapshots interfaceName current previous
            (currentRate, previousRates) <- calcDelta Dev.txRate currentSnapshot prevSnapshots
                & Maybe.catMaybes
                & unconsEither MissingPreviousInterface
            toWidget currentRate previousRates
                & pure

        InterfacePoller.GetReceiveRate InterfacePoller.NetworkRate{..} -> do
            (currentSnapshot, prevSnapshots) <- interfaceSnapshots interfaceName current previous
            (currentRate, previousRates) <- calcDelta Dev.rxRate currentSnapshot prevSnapshots
                & Maybe.catMaybes
                & unconsEither MissingPreviousInterface
            toWidget currentRate previousRates
                & pure


interfaceSnapshots :: T.Text -> [Dev.InterfaceSnapshot] -> [[Dev.InterfaceSnapshot]] -> Either Reason (Dev.InterfaceSnapshot, [Dev.InterfaceSnapshot])
interfaceSnapshots interfaceName current previous = do
    currentSnapshot <- Dev.findSnapshot interfaceName current
        & Combinators.maybeToRight (InterfaceNotFound interfaceName)
    let prevSnapshots = map (Dev.findSnapshot interfaceName) previous
            & Maybe.catMaybes
    pure (currentSnapshot, prevSnapshots)


getStatMetric :: Stat.StatSnapshot -> [Stat.StatSnapshot] -> StatPoller.Metric -> Either Reason Api.Widget
getStatMetric current previous metric =
    case metric of
        StatPoller.GetCpuUtilization StatPoller.CpuUtilization{..} -> do
            (currentUtilization, prevUtilization) <- calcDelta Stat.cpuUtilisation current previous
                & Maybe.catMaybes
                & unconsEither MissingPreviousStat
            toWidget currentUtilization prevUtilization
                & pure



calcDelta :: (a -> a -> Maybe b) -> a -> [a] -> [Maybe b]
calcDelta toRate first rest =
    let
        calc (previous, acc) current =
            (current, acc ++ [toRate previous current])
    in
    foldl calc (first, []) rest
        & snd


unconsEither :: e -> [a] -> Either e (a, [a])
unconsEither toErr list =
    case list of
        first:rest ->
            Right (first, rest)

        [] ->
            Left toErr

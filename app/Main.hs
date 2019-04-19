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
import qualified Data.Text.IO as TextIO
import qualified Data.Time.Clock as Clock
import qualified Statum.Proc.Net.Dev as Dev
import qualified Statum.Proc.Stat as Stat
import qualified Statum.Reader as Reader




data Error
    = ReadDevError Reader.Error
    | ParseDevError String
    | ReadStatError Reader.Error
    | ParseStatError String
    deriving (Show)


data Msg
    = DevMsg (Reader.Msg [Dev.InterfaceSnapshot])
    | StatMsg (Reader.Msg Stat.Stat)
    deriving (Show)



devReaderConfig :: TChan.TChan (Either Error Msg) -> Reader.Config Msg Error [Dev.InterfaceSnapshot]
devReaderConfig chan =
    Reader.Config
        { filepath = "dev.txt"
        , mapper = parseDev
        , toMsg = DevMsg
        , chan = chan
        , interval = Reader.Second 5
        , historyLength = 1
        }


statReaderConfig :: TChan.TChan (Either Error Msg) -> Reader.Config Msg Error Stat.Stat
statReaderConfig chan =
    Reader.Config
        { filepath = "stat.txt"
        , mapper = parseStat
        , toMsg = StatMsg
        , chan = chan
        , interval = Reader.Second 5
        , historyLength = 0
        }


parseDev :: Either Reader.Error Reader.Result -> Either Error [Dev.InterfaceSnapshot]
parseDev eitherResult = do
    Reader.Result{..} <- eitherResult
        & Bifunctor.first ReadDevError
    interfaces <- Dev.parse fileContents
        & Bifunctor.first ParseDevError
    interfaces
        & map (Dev.InterfaceSnapshot timestamp)
        & pure


parseStat :: Either Reader.Error Reader.Result -> Either Error Stat.Stat
parseStat eitherResult = do
    Reader.Result{..} <- eitherResult
        & Bifunctor.first ReadStatError
    Stat.parse fileContents
        & Bifunctor.first ParseStatError


main :: IO ()
main = do
    chan <- TChan.newTChan
        & STM.atomically
    chan
        & devReaderConfig
        & Reader.forkReader
    chan
        & statReaderConfig
        & Reader.forkReader
    Monad.forever $ do
        msg <- TChan.readTChan chan
            & STM.atomically
        case msg of
            Left err ->
                handleError err

            Right msg ->
                handleMsg msg
        pure ()


handleError :: Error -> IO ()
handleError err =
    print ("error", err)


-- TODO: make pure
handleMsg :: Msg -> IO ()
handleMsg msg =
    case msg of
        DevMsg Reader.Msg{..} ->
            handleDevMsg current previous

        StatMsg Reader.Msg{..} ->
            handleStatMsg current


-- TODO: make pure
handleDevMsg :: [Dev.InterfaceSnapshot] -> [[Dev.InterfaceSnapshot]] -> IO ()
handleDevMsg current previous =
    --Dev.findSnapshot current "en0"
    print current


-- TODO: make pure
handleStatMsg :: Stat.Stat -> IO ()
handleStatMsg current =
    --Dev.findSnapshot current "en0"
    print current


cpuPercent :: Either String Stat.Stat -> Either String Double
cpuPercent eitherStat = do
    Stat.Stat{..} <- eitherStat
    pure $ Stat.cpuUtilization statCpuTotal

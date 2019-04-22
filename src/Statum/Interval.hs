{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Statum.Interval
    ( Interval(..)
    , Config(..)
    , Msg(..)
    , start
    ) where


import Data.Function ((&))

import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TChan as TChan
import qualified Control.Monad as Monad
import qualified Control.Monad.State.Strict as State
import qualified Data.Either as Either


data Interval
    = Second Int
    | Minute Int
    deriving (Show)


toMicroseconds :: Interval -> Int
toMicroseconds ival =
    case ival of
        Second n ->
            n * 1000000

        Minute n ->
            n * 60 * 1000000



data Config msg e a = Config
    { action :: [a] -> IO (Either e a)
    , toMsg :: Msg a -> msg
    , chan :: TChan.TChan (Either e msg)
    , interval :: Interval
    , historyLength :: Int
    }


data Msg a = Msg
    { previous :: [a]
    , current :: a
    }
    deriving (Show)


start :: Config msg e a -> IO ()
start config =
    State.evalStateT (actionBroadcastLoop config) []
        & Concurrent.forkIO
        & Monad.void


actionBroadcastLoop :: Config msg e a -> State.StateT [a] IO ()
actionBroadcastLoop config@Config{..} =
    Monad.forever $ do
        previous <- State.get
        result <- action previous
            & State.lift
        broadcast config result previous
            & State.lift
        updateHistory historyLength result
            & State.modify'
        Concurrent.threadDelay (toMicroseconds interval)
            & State.lift


broadcast :: Config msg e a -> Either e a -> [a] -> IO ()
broadcast Config{..} result previous =
    result
        & fmap (Msg previous)
        & fmap toMsg
        & TChan.writeTChan chan
        & STM.atomically


updateHistory :: Int -> Either e a -> [a] -> [a]
updateHistory historyLength current previous =
    current
        & fmap (: previous)
        & fmap (take historyLength)
        & Either.fromRight previous

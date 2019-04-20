{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Statum.Reader
    ( Config(..)
    , Interval(..)
    , Msg(..)
    , Result(..)
    , Error(..)
    , forkReader
    ) where


import Data.Function ((&))

import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TChan as TChan
import qualified Control.Exception.Safe as Exception
import qualified Control.Monad as Monad
import qualified Control.Monad.State.Strict as State
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Either as Either
import qualified Data.Text as T
import qualified Data.Text.IO as TextIO
import qualified System.Clock as Clock


data Interval
    = Second Int
    | Minute Int
    deriving (Show)


intervalToMicroseconds :: Interval -> Int
intervalToMicroseconds ival =
    case ival of
        Second n ->
            n * 1000000

        Minute n ->
            n * 60 * 1000000



data Msg a = Msg
    { previous :: [a]
    , current :: a
    }
    deriving (Show)


data Result = Result
    { timestamp :: Clock.TimeSpec
    , fileContents :: T.Text
    }
    deriving (Show)


data Error
    = ReadError Exception.SomeException
    deriving (Show)



data Config msg e a = Config
    { filepath :: FilePath
    , mapper :: (Either Error Result -> Either e a)
    , toMsg :: (Msg a -> msg)
    , chan :: TChan.TChan (Either e msg)
    , interval :: Interval
    , historyLength :: Int
    }


forkReader :: Config msg e a -> IO ()
forkReader config =
    State.evalStateT (readBroadcastLoop config) []
        & Concurrent.forkIO
        & Monad.void


readBroadcastLoop :: Config msg e a -> State.StateT [a] IO ()
readBroadcastLoop config@Config{..} =
    Monad.forever $ do
        previous <- State.get
        result <- reader config previous
            & State.lift
        broadcast config result previous
            & State.lift
        updateHistory historyLength result
            & State.modify'
        Concurrent.threadDelay (intervalToMicroseconds interval)
            & State.lift


reader :: Config msg e a -> [a] -> IO (Either e a)
reader Config{..} previous = do
    now <- Clock.getTime Clock.Monotonic
    eitherText <- TextIO.readFile filepath
        & Exception.try
    eitherText
        & Bifunctor.first ReadError
        & fmap (Result now)
        & mapper
        & pure


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

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
    readLoop config []
        & Concurrent.forkIO
        & Monad.void



updateHistory :: Int -> [a] -> Either e a -> [a]
updateHistory historyLength history current =
    current
        & fmap (: history)
        & fmap (take historyLength)
        & Either.fromRight history


readLoop :: Config msg e a -> [a] -> IO ()
readLoop config@Config{..} previous = do
    now <- Clock.getTime Clock.Monotonic
    eitherText <- TextIO.readFile filepath
        & Exception.try
    let eitherResult = eitherText
            & Bifunctor.first ReadError
            & fmap (Result now)
            & mapper
    eitherResult
        & fmap (Msg previous)
        & fmap toMsg
        & TChan.writeTChan chan
        & STM.atomically
    Concurrent.threadDelay (intervalToMicroseconds interval)
    updateHistory historyLength previous eitherResult
        & readLoop config

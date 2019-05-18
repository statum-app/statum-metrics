{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Statum.Poller
    ( Config(..)
    , Msg(..)
    , poller
    ) where


import Data.Function ((&))
import Numeric.Natural (Natural)

import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TChan as TChan
import qualified Data.Either as Either



data Config msg e a = Config
    { action :: IO (Either e a)
    , toMsg :: Msg a -> msg
    , chan :: TChan.TChan (Either e msg)
    , historyLength :: Natural
    }


data Msg a = Msg
    { previous :: [a]
    , current :: a
    }
    deriving (Show)


poller :: Config msg e a -> [a] -> IO [a]
poller config@Config{..} previous = do
    result <- action
    broadcast config result previous
    updateHistory historyLength result previous
        & pure


broadcast :: Config msg e a -> Either e a -> [a] -> IO ()
broadcast Config{..} result previous =
    result
        & fmap (Msg previous)
        & fmap toMsg
        & TChan.writeTChan chan
        & STM.atomically


updateHistory :: Natural -> Either e a -> [a] -> [a]
updateHistory historyLength current previous =
    current
        & fmap (: previous)
        & fmap (take (fromIntegral historyLength))
        & Either.fromRight previous

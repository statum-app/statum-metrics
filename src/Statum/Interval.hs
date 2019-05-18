module Statum.Interval
    ( Interval(..)
    , startWithState
    ) where


import Data.Function ((&))
import Numeric.Natural (Natural)

import qualified Control.Concurrent as Concurrent
import qualified Control.Monad as Monad
import qualified Control.Monad.State.Strict as State


data Interval
    = Second Natural
    | Minute Natural
    deriving (Show)


toMicroseconds :: Interval -> Natural
toMicroseconds interval =
    case interval of
        Second n ->
            n * 1000000

        Minute n ->
            n * 60 * 1000000


startWithState :: Interval -> a -> (a -> IO a) -> IO ()
startWithState interval initialState action =
    State.evalStateT (foreverStateT interval action) initialState
        & Concurrent.forkIO
        & Monad.void


sleep :: Interval -> IO ()
sleep interval =
    interval
        & toMicroseconds
        & fromIntegral
        & Concurrent.threadDelay


foreverStateT :: Interval -> (a -> IO a) -> State.StateT a IO ()
foreverStateT interval action =
    Monad.forever $ do
        prevState <- State.get
        newState <- action prevState
            & State.lift
        State.put newState
        sleep interval
            & State.lift

module Statum.Interval
    ( Interval(..)
    , startWithState
    ) where


import Data.Function ((&))

import qualified Control.Concurrent as Concurrent
import qualified Control.Monad as Monad
import qualified Control.Monad.State.Strict as State


data Interval
    = Second Int
    | Minute Int
    deriving (Show)


toMicroseconds :: Interval -> Int
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


foreverStateT :: Interval -> (a -> IO a) -> State.StateT a IO ()
foreverStateT interval action =
    Monad.forever $ do
        prevState <- State.get
        newState <- action prevState
            & State.lift
        State.put newState
        Concurrent.threadDelay (toMicroseconds interval)
            & State.lift

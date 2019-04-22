module Statum.Reader
    ( Result(..)
    , Error(..)
    , reader
    ) where


import Data.Function ((&))

import qualified Control.Exception.Safe as Exception
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Text as T
import qualified Data.Text.IO as TextIO
import qualified System.Clock as Clock



data Result = Result
    { timestamp :: Clock.TimeSpec
    , fileContents :: T.Text
    }
    deriving (Show)


newtype Error = Error Exception.SomeException
    deriving (Show)



reader :: FilePath -> IO (Either Error Result)
reader filepath = do
    now <- Clock.getTime Clock.Monotonic
    eitherText <- TextIO.readFile filepath
        & Exception.try
    eitherText
        & Bifunctor.first Error
        & fmap (Result now)
        & pure

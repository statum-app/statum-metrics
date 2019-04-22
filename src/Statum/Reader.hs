{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Statum.Reader
    ( Config(..)
    , Result(..)
    , Error(..)
    , reader
    ) where


import Data.Function ((&))

import qualified Control.Exception.Safe as Exception
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Text as T
import qualified Data.Text.IO as TextIO
import qualified System.Clock as Clock



data Config e a = Config
    { filepath :: FilePath
    , mapper :: Either Error Result -> Either e a
    }


data Result = Result
    { timestamp :: Clock.TimeSpec
    , fileContents :: T.Text
    }
    deriving (Show)


data Error
    = ReadError Exception.SomeException
    deriving (Show)



reader :: Config e a -> [a] -> IO (Either e a)
reader Config{..} previous = do
    now <- Clock.getTime Clock.Monotonic
    eitherText <- TextIO.readFile filepath
        & Exception.try
    eitherText
        & Bifunctor.first ReadError
        & fmap (Result now)
        & mapper
        & pure

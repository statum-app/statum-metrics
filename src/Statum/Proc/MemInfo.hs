{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Statum.Proc.MemInfo
    ( MemInfo(..)
    , parse
    , usedPercent
    ) where

import Data.Attoparsec.Text ((<?>))
import Data.Function ((&))

import qualified Data.Attoparsec.Text as Parser
import qualified Data.Text as T
import qualified System.Clock as Clock


data MemInfo = MemInfo
    { memTotal :: Int
    , memFree :: Int
    , memAvailable :: Int
    }
    deriving (Show)



parse :: T.Text -> Either String MemInfo
parse input = do
    Parser.parseOnly memInfoParser input



usedPercent :: MemInfo -> Double
usedPercent MemInfo{..} =
    1 - (fromIntegral memFree / fromIntegral memTotal)


memInfoParser :: Parser.Parser MemInfo
memInfoParser = do
    memTotal <- memTotalParser
    memFree <- memFreeParser
    memAvailable <- memAvailableParser
    pure $ MemInfo
        { memTotal
        , memFree
        , memAvailable
        }



memTotalParser :: Parser.Parser Int
memTotalParser = do
    textParser "MemTotal:"
    kb <- kBParser
    endOfLineParser "after MemTotal"
    pure kb


memFreeParser :: Parser.Parser Int
memFreeParser = do
    textParser "MemFree:"
    kb <- kBParser
    endOfLineParser "after MemFree"
    pure kb


memAvailableParser :: Parser.Parser Int
memAvailableParser = do
    textParser "MemAvailable:"
    kb <- kBParser
    endOfLineParser "after MemAvailable"
    pure kb


kBParser :: Parser.Parser Int
kBParser = do
    Parser.skipSpace
    kb <- Parser.decimal
    Parser.skipSpace
    textParser "kB"
    pure kb


decimalParser :: Parser.Parser Int
decimalParser = do
    Parser.skipSpace
    Parser.decimal


textParser :: T.Text -> Parser.Parser T.Text
textParser text =
    Parser.string text
        <?> failureMsg text


endOfLineParser :: T.Text -> Parser.Parser ()
endOfLineParser msg =
    Parser.endOfLine
        <?> failureMsg ("endOfLine " <> msg)


failureMsg :: T.Text -> String
failureMsg name =
    [ "Failed to parse "
    , name
    ]
    & mconcat
    & T.unpack

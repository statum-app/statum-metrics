{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Statum.Proc.Net.Dev
    ( Interface(..)
    , parse
    ) where

import Data.Attoparsec.Text ((<?>))

import qualified Data.Attoparsec.Text as Parser
import Data.Function ((&))
import qualified Data.Text as T



data Interface = Interface
    { name :: T.Text
    , rxBytes :: Int
    , rxPackets :: Int
    , rxErrors :: Int
    , rxDropped :: Int
    , rxFifo :: Int
    , rxFrame :: Int
    , rxCompressed :: Int
    , rxMulticast :: Int
    , txBytes :: Int
    , txPackets :: Int
    , txErrors :: Int
    , txDropped :: Int
    , txFifo :: Int
    , txColls :: Int
    , txCarrier :: Int
    , txCompressed :: Int
    }
    deriving (Show)



parse :: T.Text -> Either String [Interface]
parse input = do
    Parser.parseOnly devParser input



skipLine :: Parser.Parser ()
skipLine = do
    Parser.skipWhile (not . Parser.isEndOfLine)
    Parser.endOfLine


devParser :: Parser.Parser [Interface]
devParser = do
    skipLine
    skipLine
    Parser.many' interfaceParser

interfaceParser :: Parser.Parser Interface
interfaceParser = do
    Parser.skipSpace
    name <- Parser.takeTill (== ':')
    Parser.char ':'
    rxBytes <- decimalParser
    rxPackets <- decimalParser
    rxErrors <- decimalParser
    rxDropped <- decimalParser
    rxFifo <- decimalParser
    rxFrame <- decimalParser
    rxCompressed <- decimalParser
    rxMulticast <- decimalParser
    txBytes <- decimalParser
    txPackets <- decimalParser
    txErrors <- decimalParser
    txDropped <- decimalParser
    txFifo <- decimalParser
    txColls <- decimalParser
    txCarrier <- decimalParser
    txCompressed <- decimalParser
    Parser.endOfLine
    pure $ Interface
        { name
        , rxBytes
        , rxPackets
        , rxErrors
        , rxDropped
        , rxFifo
        , rxFrame
        , rxCompressed
        , rxMulticast
        , txBytes
        , txPackets
        , txErrors
        , txDropped
        , txFifo
        , txColls
        , txCarrier
        , txCompressed
        }


decimalParser :: Parser.Parser Int
decimalParser = do
    Parser.skipSpace
    Parser.decimal

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Statum.Proc.Net.Dev
    ( Interface(..)
    , InterfaceSnapshot(..)
    , parse
    , txRate
    , rxRate
    , find
    , findSnapshot
    ) where

import Data.Attoparsec.Text ((<?>))
import Data.Function ((&))

import qualified Data.Attoparsec.Text as Parser
import qualified Data.List as List
import qualified Data.Text as T
import qualified System.Clock as Clock



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



data InterfaceSnapshot = InterfaceSnapshot
    { timestamp :: Clock.TimeSpec
    , interface :: Interface
    }
    deriving (Show)


find :: [Interface] -> T.Text -> Maybe Interface
find ifaces ifaceName =
    ifaces
        & List.find (\iface -> name iface == ifaceName)


findSnapshot :: [InterfaceSnapshot] -> T.Text -> Maybe InterfaceSnapshot
findSnapshot snapshots ifaceName =
    snapshots
        & List.find (\snap -> name (interface snap) == ifaceName)


rxRate :: InterfaceSnapshot -> InterfaceSnapshot -> Maybe Double
rxRate snapshotA snapshotB =
    networkRate snapshotA snapshotB rxBytes


txRate :: InterfaceSnapshot -> InterfaceSnapshot -> Maybe Double
txRate snapshotA snapshotB =
    networkRate snapshotA snapshotB txBytes


networkRate :: InterfaceSnapshot -> InterfaceSnapshot -> (Interface -> Int) -> Maybe Double
networkRate snapshotA snapshotB transferedBytes =
    let
        bytesA =
            transferedBytes (interface snapshotA)

        bytesB =
            transferedBytes (interface snapshotB)

        timestampA =
            timestamp snapshotA

        timestampB =
            timestamp snapshotB

        timeDelta =
            Clock.diffTimeSpec timestampA timestampB
                & Clock.toNanoSecs
                & fromIntegral
                & \nano -> nano / 1000000000

        rateDelta =
            (bytesA - bytesB)
                & fromIntegral

        bytesPerSecond =
           (rateDelta / timeDelta)

    in
    if bytesA >= bytesB && timestampA >= timestampB then
        Just bytesPerSecond

    else
        Nothing



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

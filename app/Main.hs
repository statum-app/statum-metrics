module Main where

import qualified Data.Text.IO as TextIO
import qualified Statum.Proc.Stat as Stat

main :: IO ()
main = do
    input <- TextIO.readFile "stat.txt"
    print $ Stat.parse input
    --print $ Stat.parse "cpu  231992 0 121126 275336733 4409 0 876 0 0 0"

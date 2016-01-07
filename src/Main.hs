module Main where

import Control.Applicative
import Data.Text
import GHC.Generics
import Jam.Types
import Jam.Decoders

jsonFile :: FilePath
jsonFile = "resources/ast/medium_class.json"

main :: IO ()
main = do
  program <- decodeFile jsonFile
  printProgram program

printProgram program = case program of
    Nothing -> putStrLn "?"
    Just ps -> print ps

module Main (main) where

import Jam.Types
import Jam.Decoders

jsonFile :: FilePath
jsonFile = "resources/ast/medium_class.json"

main :: IO ()
main = do
  program <- decodeFile jsonFile
  printProgram program

printProgram :: (Show a) => Maybe a -> IO ()
printProgram program = case program of
    Nothing -> putStrLn "?"
    Just ps -> print ps

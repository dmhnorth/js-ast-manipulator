module Main where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as B.Char8
import Control.Applicative
import Data.Aeson
import Data.Aeson.Types
import Data.Text
import GHC.Generics
import Jam.Types


jsonFile :: FilePath
jsonFile = "resources/ast/medium_class.json"

decodeFile :: FilePath -> IO (Maybe Program)
decodeFile file = decode <$> B.readFile file

decodeString :: String -> Maybe Program
decodeString s = (decode . B.Char8.pack ) s

main :: IO ()
main = do
  program <- decodeFile jsonFile
  printProgram program


printProgram program = case program of
    Nothing -> putStrLn "?"
    Just ps -> print ps

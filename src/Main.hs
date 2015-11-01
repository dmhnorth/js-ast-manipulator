module Main where

import qualified Data.ByteString.Lazy as B
import Control.Applicative
import Data.Aeson
import Data.Aeson.Types
import Data.Text
import GHC.Generics
import ParseJS.Types


jsonFile :: FilePath
jsonFile = "resources/ast/medium_class.json"

getAstString :: FilePath -> IO B.ByteString
getAstString = B.readFile

run :: FilePath -> IO (Maybe Program)
run file = decode <$> getAstString file

main :: IO ()
main = do
  d <- run jsonFile
  case d of
    Nothing -> putStrLn "?"
    Just ps -> print ps

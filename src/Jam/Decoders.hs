module Jam.Decoders where

import Control.Applicative
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as B.Char8
import Data.Aeson
import Data.Aeson.Types
import Jam.Types

decodeFile :: FilePath -> IO (Maybe Program)
decodeFile file = decode <$> B.readFile file

decodeString :: String -> Maybe Program
decodeString s = (decode . B.Char8.pack ) s

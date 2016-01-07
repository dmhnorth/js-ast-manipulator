module Jam.DecoderTests (
  tests
) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.List
import Data.Ord

import Jam.Types
import Jam.Decoders

tests :: TestTree
tests = testGroup "Tests" [unitTests]


unitTests = testGroup "No rules" [ testCase "Empty program doesn't change" $  
      decodeString "{\"type\":\"|Program\",\"body\":[]}" @?= Just (Program []) ]

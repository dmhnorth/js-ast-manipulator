import Test.Tasty
import Test.Tasty.HUnit

import Data.List
import Data.Ord

import Jam.Types
import Jam.Decoders

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]


unitTests = testGroup "No rules"
  [ testCase "Empty program doesn't change" $
      decodeString "" @?= Just (Program[])

  -- the following test does not hold
  , testCase "List comparison (same length)" $
      [1, 2, 3] `compare` [1,2,2] @?= LT
  ]

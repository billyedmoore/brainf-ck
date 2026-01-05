module Main where

import Test.Tasty
import TestOptimise qualified
import TestParse qualified

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "BrainFuck Tests" [TestParse.parseTests, TestOptimise.optimiseTests]

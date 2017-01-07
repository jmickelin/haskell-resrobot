{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Tasty
import qualified Resrobot.TypesSpec

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [Resrobot.TypesSpec.tests]

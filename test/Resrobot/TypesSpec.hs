{-# LANGUAGE OverloadedStrings #-}
module Resrobot.TypesSpec (tests) where

import Data.Aeson
import Data.Text (Text, pack)
import Data.Monoid
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Resrobot
import Resrobot.Arbitrary
import Resrobot.Types

tests :: TestTree
tests = testGroup "Resrobot.Types" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [testJSON]

testJSON :: TestTree
testJSON = testGroup "JSON parser"
           [ testGroup "eitherDecode (encode v) == Right v"
             [ QC.testProperty "DepartureBoard" $
               \ v -> eitherDecode (encode (v :: DepartureBoard)) == Right v
             , QC.testProperty "LocationList" $
               \ v -> eitherDecode (encode (v :: LocationList)) == Right v
             ]
           ]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
            [ jsonUnitTests
            ]

jsonUnitTests :: TestTree
jsonUnitTests = testGroup "JSON parser"
                [ minimalTests
                , complexTests
                ]

minimalTests :: TestTree
minimalTests = testGroup "parsing minimal examples"
               [ testCase "parsing Stop" $
                 eitherDecode stopJSON @?= Right stopParsed
               , testCase "parsing StopList" $
                 eitherDecode stoplistJSON @?= Right stoplistParsed
               , testCase "parsing Product" $
                 eitherDecode productJSON @?= Right productParsed
               , testCase "parsing Departure" $
                 eitherDecode departureJSON @?= Right departureParsed
               , testCase "parsing DepartureBoard" $
                 eitherDecode depboardJSON @?= Right depboardParsed
               , testCase "parsing StopLocation" $
                 eitherDecode stoplocJSON @?= Right stoplocParsed
               , testCase "parsing LocationList" $
                 eitherDecode locationsJSON @?= Right locationsParsed
               ]
  where
    stopJSON = "{ \"name\": \"\", \"id\": \"\", \"extId\": \"\" }"
    stopParsed = Stop "" "" ""
                 Nothing Nothing Nothing Nothing
                 Nothing Nothing Nothing Nothing
                 Nothing Nothing Nothing Nothing
                 Nothing Nothing Nothing Nothing
                 Nothing Nothing Nothing Nothing
    stoplistJSON = "{ \"Stop\": [" <> stopJSON <> ", " <> stopJSON <> "]}"
    stoplistParsed = StopList [stopParsed, stopParsed]
    productJSON = "{ \"name\": \"\", \"num\": \"\"}"
    productParsed = Product' (Just "") (Just "")
                             Nothing Nothing Nothing Nothing
                             Nothing Nothing Nothing Nothing
    departureJSON = "{ \"Product\": " <> productJSON <> ", " <>
                    "\"Stops\": " <> stoplistJSON <> ", " <>
                    "\"name\": \"\", \"type\": \"\", \"stop\": \"\", " <>
                    "\"stopid\": \"\", \"time\": \"\", \"date\": \"\"" <>
                    "}"
    departureParsed = Departure productParsed stoplistParsed
                                "" "" "" "" Nothing "" ""
                                Nothing Nothing Nothing Nothing
                                Nothing Nothing Nothing Nothing
                                Nothing Nothing Nothing
    depboardJSON = "{ \"Departure\": [" <> departureJSON <> ", " <>
                                           departureJSON <> "]}"
    depboardParsed = DepartureBoard [departureParsed, departureParsed]
                                    Nothing Nothing
    stoplocJSON = "{ \"id\": \"\", \"extId\": \"\", \"name\": \"\" }"
    stoplocParsed = StopLocation "" "" ""
                                 Nothing Nothing Nothing
                                 Nothing Nothing Nothing
    locationsJSON = "{ \"StopLocation\": [" <> stoplocJSON <> ", " <>
                                               stoplocJSON <> "]}"
    locationsParsed = LocationList [stoplocParsed, stoplocParsed]

complexTests :: TestTree
complexTests = testGroup "parsing complex examples" []

{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text (Text, pack)
import Data.Monoid
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.Aeson

import Resrobot
import Resrobot.Types

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [testJSON]

instance Arbitrary Text where
  arbitrary = pack <$> arbitrary

instance Arbitrary Stop where
  arbitrary = Stop <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                   <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                   <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                   <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                   <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                   <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary StopList where
  -- Bound length of list to speed up checks
  arbitrary = StopList <$> (arbitrary `suchThat` ((<5) . length))

instance Arbitrary Product' where
  arbitrary = Product' <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                       <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                       <*> arbitrary <*> arbitrary

instance Arbitrary Departure where
  arbitrary = Departure <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                        <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                        <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                        <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                        <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary DepartureBoard where
  -- Bound length of list to speed up checks
  arbitrary = DepartureBoard <$> (arbitrary `suchThat` ((<5) . length))
                             <*> arbitrary <*> arbitrary

instance Arbitrary StopLocation where
  arbitrary = StopLocation <$> arbitrary <*> arbitrary <*> arbitrary
                           <*> arbitrary <*> arbitrary <*> arbitrary
                           <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary LocationList where
  arbitrary = LocationList <$> arbitrary

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

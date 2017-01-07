module Resrobot.Arbitrary where

import Data.Text (Text, pack)
import Test.Tasty.QuickCheck

import Resrobot
import Resrobot.Types

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

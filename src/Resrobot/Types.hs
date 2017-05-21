{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Resrobot.Types where

import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Char
import Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics
import Lens.Micro

data Stop = Stop { stopName :: Text

                 , stopIdentifier :: Text
                 , stopExtId    :: Text
                 , stopRouteIdx :: Maybe Integer

                 , stopLat :: Maybe Float
                 , stopLon :: Maybe Float

                 , stopDepTime :: Maybe Text
                 , stopDepData :: Maybe Text
                 , stopDepTz   :: Maybe Integer

                 , stopArrTime :: Maybe Text
                 , stopArrData :: Maybe Text
                 , stopArrTz   :: Maybe Integer

                 , stopArrTrack :: Maybe Text
                 , stopDepTrack :: Maybe Text

                 , stopRtDepTime :: Maybe Text
                 , stopRtDepDate :: Maybe Text
                 , stopRtDepTz   :: Maybe Integer

                 , stopRtArrTime :: Maybe Text
                 , stopRtArrDate :: Maybe Text
                 , stopRtArrTz   :: Maybe Integer

                 , stopRtArrTrack :: Maybe Text
                 , stopRtDepTrack :: Maybe Text

                 , stopCancelled :: Maybe Bool
                 }
            deriving (Show, Generic, Eq)

newtype StopList = StopList { stoplistStop :: [Stop] }
             deriving (Show, Generic, Eq, Monoid)

data ResProduct = ResProduct { prodName :: Maybe Text
                         , prodNum  :: Maybe Text

                         , prodCatIn   :: Maybe Text
                         , prodCatCode :: Maybe Text
                         , prodNatOutS :: Maybe Text
                         , prodNatOutL :: Maybe Text

                         , prodOperatorCode :: Maybe Text
                         , prodOperator     :: Maybe Text
                         , prodOperatorUrl  :: Maybe Text

                         , prodAdmin :: Maybe Text
                         }
              deriving (Show, Generic, Eq)

data Departure = Departure { depProduct :: ResProduct
                           , depStops   :: StopList

                           , depName  :: Text
                           , depDType :: Text

                           , depStop      :: Text
                           , depStopid    :: Text
                           , depStopExtId :: Maybe Text

                           , depTime :: Text
                           , depDate :: Text

                           , depTz    :: Maybe Integer
                           , depTrack :: Maybe Text

                           , depRtTime     :: Maybe Text
                           , depRtTrack    :: Maybe Text
                           , depRtDepTrack :: Maybe Text
                           , depRtDate     :: Maybe Text
                           , depRtTz       :: Maybe Integer

                           , depCancelled :: Maybe Bool

                           , depDirection :: Maybe Text

                           , depTransportNumber   :: Maybe Text
                           , depTransportCategory :: Maybe Text

                           }
            deriving (Show, Generic, Eq)

data DepartureBoard = DepartureBoard { boardDeparture :: [Departure]
                                     , boardErrorCode :: Maybe Text
                                     , boardErrorText :: Maybe Text
                                     }
            deriving (Show, Generic, Eq)

data StopLocation = StopLocation { stoplocIdentifier :: Text
                                 , stoplocExtId :: Text
                                 , stoplocName :: Text

                                 , stoplocLon :: Maybe Float
                                 , stoplocLat :: Maybe Float

                                 , stoplocTrack :: Maybe Text

                                 , stoplocWeight :: Maybe Integer
                                 , stoplocDist :: Maybe Integer

                                 , stoplocProducts :: Maybe Integer
                                 }
                    deriving(Show, Generic, Eq)

newtype LocationList = LocationList { locationsStopLocation :: [StopLocation] }
                   deriving(Show, Generic, Eq, Monoid)

------------------------------------------------------------
-- Instances
------------------------------------------------------------

lowercaseFirst = over _head toLower
dropPrefix = dropWhile isLower

{- Stop

   JSON fields are camelCase

   The "id" field is renamed "stopIdentifier" to avoid clashing
   with the built-in "id" function.
-}
stopModifier "stopIdentifier" = "id"
stopModifier s = lowercaseFirst . dropPrefix $ s
instance FromJSON Stop where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = stopModifier
    }

instance ToJSON Stop where
  toEncoding = genericToEncoding defaultOptions {
    fieldLabelModifier = stopModifier
    }

{- StopList

   JSON field is on the form "Stop"
-}
instance FromJSON StopList where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = dropPrefix
    }

instance ToJSON StopList where
  toEncoding = genericToEncoding defaultOptions {
    fieldLabelModifier = dropPrefix
    }

{- ResProduct

   JSON fields are camelCase
-}
instance FromJSON ResProduct where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = lowercaseFirst . dropPrefix
    }

instance ToJSON ResProduct where
  toEncoding = genericToEncoding defaultOptions {
    fieldLabelModifier = lowercaseFirst . dropPrefix
    }

{- Departure

   JSON fields are camelCase except for Product and Stops

   The "type" field is renamed "depDType" to avoid clashing
   with the "type" keyword when generating lenses for this
   type.
-}
departureModifier "depProduct" = "Product"
departureModifier "depStops"   = "Stops"
departureModifier "depDType"   = "type"
departureModifier s = lowercaseFirst . dropPrefix $ s

instance FromJSON Departure where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = departureModifier
    }

instance ToJSON Departure where
  toEncoding = genericToEncoding defaultOptions {
    fieldLabelModifier = departureModifier
    }

{- DepartureBoard

  JSON fields are camelCase except for Departure
-}
departureBoardModifier "boardDeparture" = "Departure"
departureBoardModifier s = lowercaseFirst . dropPrefix $ s

instance FromJSON DepartureBoard where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = departureBoardModifier
    }

instance ToJSON DepartureBoard where
  toEncoding = genericToEncoding defaultOptions {
    fieldLabelModifier = departureBoardModifier
    }

{- StopLocation

   JSON fields are camelCase

   The "id" field is renamed "stoplocIdentifier" to avoid clashing
   with the built-in "id" function.
-}
stoplocationModifier "stoplocIdentifier" = "id"
stoplocationModifier s = lowercaseFirst . dropPrefix $ s
instance FromJSON StopLocation where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = stoplocationModifier
    }

instance ToJSON StopLocation where
  toEncoding = genericToEncoding defaultOptions {
    fieldLabelModifier = stoplocationModifier
    }

{- LocationList

   JSON field is on the form "StopLocation"
-}
instance FromJSON LocationList where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = dropPrefix
    }

instance ToJSON LocationList where
  toEncoding = genericToEncoding defaultOptions {
    fieldLabelModifier = dropPrefix
    }

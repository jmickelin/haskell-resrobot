{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Resrobot.Types where

import Control.Lens
import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Char
import Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics

data Stop = Stop { stopName :: Text

                 , stopId       :: Text
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

data Product' = Product' { prodName :: Maybe Text
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

data Departure = Departure { depProduct :: Product'
                           , depStops   :: StopList

                           , depName  :: Text
                           , depType :: Text

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

data StopLocation = StopLocation { stoplocId :: Text
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
-}
instance FromJSON Stop where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = lowercaseFirst . dropPrefix
    }

instance ToJSON Stop where
  toEncoding = genericToEncoding defaultOptions {
    fieldLabelModifier = lowercaseFirst . dropPrefix
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

{- Product'

   JSON fields are camelCase
-}
instance FromJSON Product' where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = lowercaseFirst . dropPrefix
    }

instance ToJSON Product' where
  toEncoding = genericToEncoding defaultOptions {
    fieldLabelModifier = lowercaseFirst . dropPrefix
    }

{- Departure

   JSON fields are camelCase except for Product and Stops
-}
departureModifier "depProduct" = "Product"
departureModifier "depStops"   = "Stops"
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
-}
instance FromJSON StopLocation where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = lowercaseFirst . dropPrefix
    }

instance ToJSON StopLocation where
  toEncoding = genericToEncoding defaultOptions {
    fieldLabelModifier = lowercaseFirst . dropPrefix
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

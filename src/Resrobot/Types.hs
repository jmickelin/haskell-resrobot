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

lowercaseFirst = over _head toLower
dropPrefix = dropWhile isLower

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

instance FromJSON Stop where
  -- JSON fields are camelCase
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = lowercaseFirst . dropPrefix
    }

instance ToJSON Stop where
  -- JSON fields are camelCase
  toEncoding = genericToEncoding defaultOptions {
    fieldLabelModifier = lowercaseFirst . dropPrefix
    }

newtype StopList = StopList { stoplistStop :: [Stop] }
             deriving (Show, Generic, Eq, Monoid)

instance FromJSON StopList where
  -- JSON field is on the form "Stop"
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = dropPrefix
    }

instance ToJSON StopList where
  -- JSON field is on the form "Stop"
  toEncoding = genericToEncoding defaultOptions {
    fieldLabelModifier = dropPrefix
    }

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

instance FromJSON Product' where
  -- JSON fields are camelCase
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = lowercaseFirst . dropPrefix
    }

instance ToJSON Product' where
  -- JSON fields are camelCase
  toEncoding = genericToEncoding defaultOptions {
    fieldLabelModifier = lowercaseFirst . dropPrefix
    }

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

-- JSON fields are camelCase except for Product and Stops
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

data DepartureBoard = DepartureBoard { boardDeparture :: [Departure]
                                     , boardErrorCode :: Maybe Text
                                     , boardErrorText :: Maybe Text
                                     }
            deriving (Show, Generic, Eq)

-- JSON fields are camelCase except for Departure
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

instance FromJSON StopLocation where
  -- JSON fields are camelCase
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = lowercaseFirst . dropPrefix
    }

instance ToJSON StopLocation where
  -- JSON fields are camelCase
  toEncoding = genericToEncoding defaultOptions {
    fieldLabelModifier = lowercaseFirst . dropPrefix
    }

newtype LocationList = LocationList { locationsStopLocation :: [StopLocation] }
                   deriving(Show, Generic, Eq, Monoid)

instance FromJSON LocationList where
  -- JSON field is on the form "StopLocation"
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = dropPrefix
    }

instance ToJSON LocationList where
  -- JSON field is on the form "StopLocation"
  toEncoding = genericToEncoding defaultOptions {
    fieldLabelModifier = dropPrefix
    }

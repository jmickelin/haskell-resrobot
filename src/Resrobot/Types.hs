{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Resrobot.Types where

import Control.Lens
import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Char
import Data.Text (Text)
import qualified Data.Text as T

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
            deriving (Show, Eq)

$(deriveJSON defaultOptions {
     -- JSON fields are camelCase
     fieldLabelModifier = (_head %~ toLower) . dropWhile isLower
     } ''Stop)

newtype StopList = StopList { stoplistStop :: [Stop] }
             deriving (Show, Eq, Monoid)

$(deriveJSON defaultOptions {
     -- JSON field is on the form "Stop"
     fieldLabelModifier = dropWhile isLower
     } ''StopList)

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
              deriving (Show, Eq)

$(deriveJSON defaultOptions {
     -- JSON fields are camelCase
     fieldLabelModifier = (_head %~ toLower) . dropWhile isLower
     } ''Product')

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
            deriving (Show, Eq)

$(deriveJSON defaultOptions {
     -- JSON fields are camelCase except for Product and Stops
     fieldLabelModifier = \s -> if s `elem` ["depProduct", "depStops"]
                                then dropWhile isLower s
                                else (_head %~ toLower) . dropWhile isLower $ s
     } ''Departure)

data DepartureBoard = DepartureBoard { boardDeparture :: [Departure]
                                     , boardErrorCode :: Maybe Text
                                     , boardErrorText :: Maybe Text
                                     }
            deriving (Show, Eq)

$(deriveJSON defaultOptions {
     -- JSON fields are camelCase except for Departures
     fieldLabelModifier = \s -> if s `elem` ["boardDeparture"]
                                then dropWhile isLower s
                                else (_head %~ toLower) . dropWhile isLower $ s
     } ''DepartureBoard)

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
                    deriving(Show, Eq)

$(deriveJSON defaultOptions {
     -- JSON fields are camelCase
     fieldLabelModifier = (_head %~ toLower) . dropWhile isLower
     } ''StopLocation)

newtype LocationList = LocationList { locationsStopLocation :: [StopLocation] }
                   deriving(Show, Eq, Monoid)

$(deriveJSON defaultOptions {
     -- JSON field is on the form "StopLocation"
     fieldLabelModifier = dropWhile isLower
     } ''LocationList)

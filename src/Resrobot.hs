{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Resrobot ( departures
                , locations
                , Format(..)
                , module Resrobot.Types
                ) where

import Data.Proxy
import Data.Text (Text)
import Servant.API
import Servant.Client
import Web.HttpApiData

import Resrobot.Types

type TimetableAPI = "v2" :> "departureBoard.json"
                         :> QueryParam "key" Text
                         :> QueryParam "id" Text
                         :> Get '[JSON] DepartureBoard
                    :<|> "v2" :> "location.name.json"
                              :> QueryParam "key" Text
                              :> QueryParam "input" Text
                              :> QueryParam "format" Format
                              :> Get '[JSON] LocationList

data Format = Json | Xml
              deriving (Show)

instance ToHttpApiData Format where
  toUrlPiece = showTextData

timetableApi :: Proxy TimetableAPI
timetableApi = Proxy

departures' :<|> locations' = client timetableApi

departures :: Text -> Text -> ClientM DepartureBoard
departures key _id = departures' (Just key) (Just _id)

locations :: Text -> Text -> ClientM LocationList
locations key input = locations' (Just key) (Just input) (Just Json)

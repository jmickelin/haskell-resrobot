{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Resrobot ( getDepartures
                , getLocation
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

getDepartures :<|> getLocation = client timetableApi

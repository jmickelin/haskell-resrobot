module Resrobot.Utils where

import Control.Lens
import Data.Maybe
import Data.Text (Text)
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.HTTP.Client.TLS
import Servant.API
import Servant.Client

import Resrobot
import Resrobot.Lens

defaultOptions = BaseUrl { baseUrlScheme = Https
                         , baseUrlHost = "api.resrobot.se"
                         , baseUrlPort = 443
                         , baseUrlPath = ""
                         }

runClientMWith :: BaseUrl -> ClientM a -> IO (Either ServantError a)
runClientMWith baseurl action = do
  manager <- newManager tlsManagerSettings
  let env = ClientEnv manager baseurl
  runClientM action env

runClientMDefault :: ClientM a -> IO (Either ServantError a)
runClientMDefault = runClientMWith defaultOptions

getLocations :: Text -> Text -> IO (Either ServantError LocationList)
getLocations key name = runClientMDefault (locations key name)

getDepartures :: Text -> Text -> IO (Either ServantError DepartureBoard)
getDepartures key name = runClientMDefault (departures key name)

-- The API states that the list of returned locations is non-empty, so
-- this should be fine. It would still be good form to make it total.
topLocation :: LocationList -> StopLocation
topLocation = head . locationsStopLocation

getTopLocation :: Text -> Text -> IO (Either ServantError StopLocation)
getTopLocation key name = getTop <$> getLocations key name
  where
    getTop = over _Right topLocation

getDeparturesByName :: Text -> Text -> Text -> IO (Either ServantError DepartureBoard)
getDeparturesByName locationKey departureKey name = runClientMDefault $
  do l <- locations locationKey name
     let locId = topLocation l ^. _stoplocId
     departures departureKey locId

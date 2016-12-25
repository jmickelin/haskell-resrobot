{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
module Resrobot.Lens where

import Control.Lens.TH
import qualified Resrobot.Types as RT

makeClassy_ ''RT.Stop
makeClassy_ ''RT.StopList
makeClassy_ ''RT.Product'
makeClassy_ ''RT.Departure
makeClassy_ ''RT.DepartureBoard
makeClassy_ ''RT.StopLocation
makeClassy_ ''RT.LocationList

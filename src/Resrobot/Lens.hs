{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# OPTIONS_HADDOCK prune #-}
module Resrobot.Lens where

import           Data.Foldable
import           Lens.Micro
import           Lens.Micro.TH
import qualified Resrobot.Types as RT

makeLensesWith abbreviatedFields ''RT.Stop
makeLensesWith abbreviatedFields ''RT.StopList
makeLensesWith abbreviatedFields ''RT.ResProduct
makeLensesWith abbreviatedFields ''RT.Departure
makeLensesWith abbreviatedFields ''RT.DepartureBoard
makeLensesWith abbreviatedFields ''RT.StopLocation
makeLensesWith abbreviatedFields ''RT.LocationList

module Database.Stormpath
( STORMPATH
, APIKey
, Client
, Application

, newAPIKey
, newClient
, getApplication
) where

import NN.Prelude

foreign import data STORMPATH :: !
foreign import data APIKey :: *
foreign import data Client :: *
foreign import data Application :: *
foreign import newAPIKey :: String -> String -> APIKey
foreign import newClient :: ∀ eff. APIKey -> Aff (stormpath :: STORMPATH | eff) Client
foreign import getApplication :: ∀ eff. Client -> String -> Aff (stormpath :: STORMPATH | eff) Application

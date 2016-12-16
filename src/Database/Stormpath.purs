module Database.Stormpath
( STORMPATH
, APIKey
, Client
, Application
, Account

, newAPIKey
, newClient
, getApplication
, authenticateAccount
, accountHref
) where

import NN.Prelude

foreign import data STORMPATH :: !
foreign import data APIKey :: *
foreign import data Client :: *
foreign import data Application :: *
foreign import data Account :: *
foreign import newAPIKey :: String -> String -> APIKey
foreign import newClient :: ∀ eff. APIKey -> Aff (stormpath :: STORMPATH | eff) Client
foreign import getApplication :: ∀ eff. Client -> String -> Aff (stormpath :: STORMPATH | eff) Application
foreign import authenticateAccount :: ∀ eff. Application -> String -> String -> Aff (stormpath :: STORMPATH | eff) Account
foreign import accountHref :: Account -> String

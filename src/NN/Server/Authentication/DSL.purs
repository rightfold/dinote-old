module NN.Server.Authentication.DSL
( AuthenticationDSL
, AuthenticationDSLF(..)
, authenticate
) where

import Control.Monad.Free (Free, liftF)
import Data.Password (Password)
import NN.Prelude
import NN.Session (SessionID)
import NN.User (UserID)

type AuthenticationDSL = Free AuthenticationDSLF

data AuthenticationDSLF a
    = Authenticate String Password (Maybe (SessionID × UserID) -> a)

authenticate :: String -> Password -> AuthenticationDSL (Maybe (SessionID × UserID))
authenticate username password = liftF $ Authenticate username password id

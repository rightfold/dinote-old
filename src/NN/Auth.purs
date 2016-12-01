module NN.Auth
( UserID(..)
) where

import Data.Generic (class Generic, gShow)
import NN.Prelude

data UserID
    = AnonymousID
    | RegisteredID String

derive instance genericUserID :: Generic UserID
derive instance eqUserID :: Eq UserID
derive instance ordUserID :: Ord UserID
instance showUserID :: Show UserID where show = gShow

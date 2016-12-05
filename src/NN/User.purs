module NN.User
( UserID(..)
) where

import Data.Generic (class Generic, gShow)
import Data.Sexp (class AsSexp, gFromSexp, gToSexp, class FromSexp, class ToSexp)
import NN.Prelude

newtype UserID = UserID String

derive instance genericUserID :: Generic UserID
derive instance eqUserID :: Eq UserID
derive instance ordUserID :: Ord UserID
instance fromSexpUserID :: FromSexp UserID where fromSexp = gFromSexp
instance toSexpUserID :: ToSexp UserID where toSexp = gToSexp
instance asSexpUserID :: AsSexp UserID
instance showUserID :: Show UserID where show = gShow

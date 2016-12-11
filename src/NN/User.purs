module NN.User
( UserID(..)
) where

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Sexp (class AsSexp, genericFromSexp, genericToSexp, class FromSexp, class ToSexp)
import NN.Prelude

newtype UserID = UserID String

derive instance genericUserID :: Generic UserID _
derive instance eqUserID :: Eq UserID
derive instance ordUserID :: Ord UserID
instance fromSexpUserID :: FromSexp UserID where fromSexp = genericFromSexp
instance toSexpUserID :: ToSexp UserID where toSexp = genericToSexp
instance asSexpUserID :: AsSexp UserID
instance showUserID :: Show UserID where show = genericShow

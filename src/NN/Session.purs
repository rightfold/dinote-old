module NN.Session
( SessionID(..)
) where

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Sexp (class AsSexp, genericFromSexp, genericToSexp, class FromSexp, class ToSexp)
import NN.Prelude

newtype SessionID = SessionID String

derive instance genericSessionID :: Generic SessionID _
derive instance eqSessionID :: Eq SessionID
derive instance ordSessionID :: Ord SessionID
instance fromSexpSessionID :: FromSexp SessionID where fromSexp = genericFromSexp
instance toSexpSessionID :: ToSexp SessionID where toSexp = genericToSexp
instance asSexpSessionID :: AsSexp SessionID
instance showSessionID :: Show SessionID where show = genericShow

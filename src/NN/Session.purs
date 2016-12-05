module NN.Session
( SessionID(..)
) where

import Data.Generic (class Generic, gShow)
import Data.Sexp (class AsSexp, gFromSexp, gToSexp, class FromSexp, class ToSexp)
import NN.Prelude

newtype SessionID = SessionID String

derive instance genericSessionID :: Generic SessionID
derive instance eqSessionID :: Eq SessionID
derive instance ordSessionID :: Ord SessionID
instance fromSexpSessionID :: FromSexp SessionID where fromSexp = gFromSexp
instance toSexpSessionID :: ToSexp SessionID where toSexp = gToSexp
instance asSexpSessionID :: AsSexp SessionID
instance showSessionID :: Show SessionID where show = gShow

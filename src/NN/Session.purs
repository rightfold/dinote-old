module NN.Session
( SessionID(..)
, Session(..)
, sessionUserID
, sessionDescription
) where

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens', lens)
import Data.Sexp (class AsSexp, genericFromSexp, genericToSexp, class FromSexp, class ToSexp)
import NN.Prelude
import NN.User (UserID)

newtype SessionID = SessionID String

derive instance genericSessionID :: Generic SessionID _
derive instance eqSessionID :: Eq SessionID
derive instance ordSessionID :: Ord SessionID
derive instance newtypeUserID :: Newtype SessionID _
instance fromSexpSessionID :: FromSexp SessionID where fromSexp = genericFromSexp
instance toSexpSessionID :: ToSexp SessionID where toSexp = genericToSexp
instance asSexpSessionID :: AsSexp SessionID
instance showSessionID :: Show SessionID where show = genericShow

data Session = Session UserID String

sessionUserID :: Lens' Session UserID
sessionUserID = lens (\(Session a _) -> a) (\(Session _ b) a -> Session a b)

sessionDescription :: Lens' Session String
sessionDescription = lens (\(Session _ b) -> b) (\(Session a _) b -> Session a b)


module NN.Auth
( UserID(..)
) where

import NN.Prelude

data UserID
    = AnonymousID
    | RegisteredID String

derive instance eqUserID :: Eq UserID
derive instance ordUserID :: Ord UserID

instance showUserID :: Show UserID where
    show AnonymousID = "AnonymousID"
    show (RegisteredID v) = "(RegisteredID " <> show v <> ")"

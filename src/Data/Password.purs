module Data.Password
( Password(..)
) where

import Data.Sexp (class FromSexp, Sexp(..), class ToSexp)
import NN.Prelude

newtype Password = Password String

derive newtype instance eqPassword :: Eq Password
derive newtype instance ordPassword :: Ord Password
derive newtype instance fromSexpPassword :: FromSexp Password
instance toSexpPassword :: Fail "Do not serialize passwords." => ToSexp Password where toSexp = const $ Atom ""
instance showPassword :: Show Password where show = const $ "(Password " <> show "hunter2" <> ")"

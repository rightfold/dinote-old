module NN.Note
( Note(..)
) where

import NN.Prelude

data Note
    = Empty
    | Append Note Note

instance semigroupNote :: Semigroup Note where
    append = Append

instance monoidNote :: Monoid Note where
    mempty = Empty

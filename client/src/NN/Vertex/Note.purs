module NN.Vertex.Note
( Note(..)
) where

import NN.Prelude

data Note
    = Empty
    | Append Note Note
    | Text String

instance semigroupNote :: Semigroup Note where
    append = Append

instance monoidNote :: Monoid Note where
    mempty = Empty

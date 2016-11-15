module NN.Prelude
( module Control.Monad.Aff
, module Control.Monad.Eff
, module Data.Foldable
, module Data.Lazy
, module Data.List
, module Data.Maybe
, module Data.Monoid
, module Data.Traversable
, module Data.Tuple
, module Prelude
, type (⊕)
, type (⊗)
) where

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Data.Foldable (fold, class Foldable, foldl, foldr)
import Data.Functor.Coproduct (Coproduct)
import Data.Functor.Product (Product)
import Data.Lazy (defer, force, Lazy)
import Data.List ((:), List(Nil))
import Data.Maybe (fromMaybe, Maybe(..), maybe)
import Data.Monoid (mempty, class Monoid)
import Data.Traversable (for, for_, class Traversable, traverse, traverse_)
import Data.Tuple (curry, fst, snd, Tuple(..), uncurry)
import Prelude

infixr 6 type Coproduct as ⊕
infixr 7 type Product as ⊗

module NN.Prelude
( module Control.Monad.Aff
, module Control.Monad.Aff.Class
, module Control.Monad.Eff
, module Control.Monad.Eff.Class
, module Data.Either
, module Data.Functor.Coproduct
, module Data.List
, module Data.Maybe
, module Data.Monoid
, module Data.Newtype
, module Data.Traversable
, module Data.Tuple
, module Data.Tuple.Nested
, module Debug.Trace
, module Prelude
, type (⊕)
, liftEff'
) where

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Data.Either (Either(..))
import Data.Functor.Coproduct (Coproduct)
import Data.List ((:), List(Nil))
import Data.Maybe (fromJust, fromMaybe, Maybe(..), maybe)
import Data.Monoid (mempty, class Monoid)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Traversable (for, for_, traverse)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Debug.Trace (trace, traceShow, traceAny, spy, traceAnyA, traceA, traceShowA, traceAnyM, traceShowM)
import Prelude

infixr 6 type Coproduct as ⊕

liftEff' :: ∀ a eff. Eff (err :: EXCEPTION | eff) a -> Aff eff a
liftEff' = liftEff <<< unsafeCoerceEff

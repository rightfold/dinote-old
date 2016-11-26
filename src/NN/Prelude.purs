module NN.Prelude
( module Control.Monad.Aff
, module Control.Monad.Aff.Class
, module Control.Monad.Eff
, module Control.Monad.Eff.Class
, module Control.Monad.Eff.Unsafe
, module Control.Monad.Free
, module Control.Monad.Rec.Class
, module Control.Monad.Trans.Class
, module Data.Either
, module Data.Foldable
, module Data.Functor.Coproduct
, module Data.Functor.Product
, module Data.Generic
, module Data.Lazy
, module Data.List
, module Data.Maybe
, module Data.Monoid
, module Data.String.CaseInsensitive
, module Data.Traversable
, module Data.Tuple
, module Data.Tuple.Nested
, module Debug.Trace
, module Partial.Unsafe
, module Prelude
, type (⊕)
, type (⊗)
, (∈)
, (\)
, (∪)
, (∩)
, liftEff'
) where

import Control.Monad.Aff (Aff, forkAff, launchAff, runAff)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff, unsafePerformEff)
import Control.Monad.Free (foldFree, Free, hoistFree, liftF)
import Control.Monad.Rec.Class (forever)
import Control.Monad.Trans.Class (lift, class MonadTrans)
import Data.Either (Either(..))
import Data.Foldable (fold, class Foldable, foldl, foldr)
import Data.Functor.Coproduct (Coproduct, coproduct, left, right)
import Data.Functor.Product (Product)
import Data.Generic (class Generic, gShow)
import Data.Lazy (defer, force, Lazy)
import Data.List ((:), List(Nil))
import Data.Maybe (fromJust, fromMaybe, Maybe(..), maybe)
import Data.Monoid (mempty, class Monoid)
import Data.String.CaseInsensitive (CaseInsensitiveString(..))
import Data.Set as Set
import Data.Traversable (for, for_, class Traversable, traverse, traverse_)
import Data.Tuple (curry, fst, snd, Tuple(..), uncurry)
import Data.Tuple.Nested ((/\))
import Debug.Trace (trace, traceShow, traceAny, spy, traceAnyA, traceA, traceShowA, traceAnyM, traceShowM)
import Partial.Unsafe (unsafePartial)
import Prelude

infixr 6 type Coproduct as ⊕
infixr 7 type Product as ⊗

infix 4 Set.member as ∈
infixl 6 Set.difference as \
infixl 6 Set.union as ∪
infixl 7 Set.intersection as ∩

liftEff' :: ∀ a eff. Eff (err :: EXCEPTION | eff) a -> Aff eff a
liftEff' = liftEff <<< unsafeCoerceEff

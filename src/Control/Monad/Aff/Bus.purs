{-
Copyright 2016 SlamData, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

module Control.Monad.Aff.Bus
  ( make
  , read
  , write
  , split
  , Cap
  , Bus
  , BusRW
  , BusR
  , BusW
  ) where

import Prelude
import Control.Monad.Aff (forkAll, forkAff)
import Control.Monad.Aff.AVar (AffAVar, makeVar', makeVar, takeVar, putVar, modifyVar)
import Control.Monad.Rec.Class (forever)
import Data.Foldable (foldl)
import Data.List ((:))
import Data.Monoid (mempty)
import Data.Tuple (Tuple(..))

data Cap

data Bus (r ∷ # *) a = Bus (∀ eff. a → AffAVar eff Unit) (∀ eff. AffAVar eff a)

type BusRW = Bus (read ∷ Cap, write ∷ Cap)

type BusR = Bus (read ∷ Cap)

type BusW = Bus (write ∷ Cap)

-- | Creates a new bidirectional Bus which can be read from and written to.
make ∷ ∀ eff a. AffAVar eff (BusRW a)
make = do
  cell ← makeVar
  consumers ← makeVar' mempty

  forkAff $ forever do
    res ← takeVar cell
    fns ← takeVar consumers
    putVar consumers mempty
    forkAll (foldl (\xs f → f res : xs) mempty fns)

  pure $ Bus (putVar cell) do
    res' ← makeVar
    modifyVar (putVar res' : _) consumers
    takeVar res'

-- | Splits a bidirectional Bus into separate read and write Buses.
split ∷ ∀ a. BusRW a → Tuple (BusR a) (BusW a)
split (Bus w r) = Tuple (Bus w r) (Bus w r)

-- | Blocks until a new value is pushed to the Bus, returning the value.
read ∷ ∀ eff a r. Bus (read ∷ Cap | r) a → AffAVar eff a
read (Bus _ r) = r

-- | Pushes a new value to the Bus, yielding immediately.
write ∷ ∀ eff a r. a → Bus (write ∷ Cap | r) a → AffAVar eff Unit
write a (Bus w _) = w a

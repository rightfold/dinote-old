module NN.Monad
( NN
, NNF(..)
, runNN
) where

import Control.Monad.Free (foldFree, Free)
import NN.Prelude

type NN = Free NNF

data NNF a = NNF Void

runNN :: forall eff. NN ~> Aff eff
runNN = foldFree go
    where
    go :: NNF ~> Aff eff
    go (NNF void) = absurd void

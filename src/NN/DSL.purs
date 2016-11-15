module NN.DSL
( NNDSL
, NNDSLF(..)
, runNNDSL
) where

import Control.Monad.Free (foldFree, Free)
import NN.Prelude

type NNDSL = Free NNDSLF

data NNDSLF a = NNDSLF Void

runNNDSL :: ∀ eff. NNDSL ~> Aff eff
runNNDSL = foldFree go
    where
    go :: NNDSLF ~> Aff eff
    go (NNDSLF void) = absurd void

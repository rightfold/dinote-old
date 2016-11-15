module NN.DSL
( NNDSL
, NNDSLF
, runNNDSL
) where

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Bus as Bus
import Control.Monad.Free (foldFree, Free)
import NN.Prelude
import NN.Vertex.DSL (VertexDSLF(..))

type NNDSL = Free NNDSLF

type NNDSLF = VertexDSLF

runNNDSL :: ∀ eff. NNDSL ~> Aff (avar :: AVAR | eff)
runNNDSL = foldFree go
    where
    go :: NNDSLF ~> Aff (avar :: AVAR | eff)
    go (Get _ a) = pure $ a Nothing
    go (Bus _ a) = a <$> Bus.make

module NN.Client.DSL.Interpret
( interpret
, runNNDSL
, runVertexDSL
) where

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Bus (BusRW)
import Control.Monad.Free (foldFree, Free)
import Data.Functor.Coproduct (coproduct)
import Network.HTTP.Affjax (AJAX)
import NN.Client.DSL (NNDSL)
import NN.Prelude
import NN.Vertex (Vertex, VertexID)
import NN.Client.Vertex.HTTP (createEdge, createVertex, getVertex)
import NN.Client.Vertex.DSL (VertexDSL, VertexDSLF(..))

interpret
    :: ∀ eff
     . BusRW (Tuple VertexID Vertex)
    -> Free (Aff (ajax :: AJAX, avar :: AVAR | eff) ⊕ NNDSL)
    ~> Aff (ajax :: AJAX, avar :: AVAR | eff)
interpret vertexBus = foldFree (coproduct id (runNNDSL vertexBus))

runNNDSL
    :: ∀ eff
     . BusRW (Tuple VertexID Vertex)
    -> NNDSL
    ~> Aff (ajax :: AJAX, avar :: AVAR | eff)
runNNDSL = runVertexDSL

runVertexDSL
    :: ∀ eff
     . BusRW (Tuple VertexID Vertex)
    -> VertexDSL
    ~> Aff (ajax :: AJAX, avar :: AVAR | eff)
runVertexDSL vertexBus = foldFree go
    where
    go :: VertexDSLF ~> Aff (ajax :: AJAX, avar :: AVAR | eff)
    go (GetVertex fileID vertexID a) = a <$> getVertex fileID vertexID
    go (VertexBus a) = pure $ a vertexBus
    go (CreateVertex fileID a) = a <$> createVertex fileID
    go (CreateEdge fileID edge a) = a <$ createEdge fileID edge

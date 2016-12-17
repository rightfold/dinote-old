module NN.Client.Vertex.DSL.Interpret.HTTP
( runVertexDSL
, runVertexDSLF
) where

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Bus (BusRW)
import Control.Monad.Free (foldFree)
import Network.HTTP.Affjax (AJAX)
import NN.Prelude
import NN.Vertex (Vertex, VertexID)
import NN.Client.Vertex.HTTP (createEdge, createVertex, getVertex)
import NN.Client.Vertex.DSL (VertexDSL, VertexDSLF(..))

runVertexDSL
    :: ∀ eff
     . BusRW (Tuple VertexID Vertex)
    -> VertexDSL
    ~> Aff (ajax :: AJAX, avar :: AVAR | eff)
runVertexDSL = foldFree <<< runVertexDSLF

runVertexDSLF
    :: ∀ eff
     . BusRW (Tuple VertexID Vertex)
    -> VertexDSLF
    ~> Aff (ajax :: AJAX, avar :: AVAR | eff)
runVertexDSLF _ (GetVertex fileID vertexID a) = a <$> getVertex fileID vertexID
runVertexDSLF vertexBus (VertexBus a) = pure $ a vertexBus
runVertexDSLF _ (CreateVertex fileID a) = a <$> createVertex fileID
runVertexDSLF _ (CreateEdge fileID edge a) = a <$ createEdge fileID edge


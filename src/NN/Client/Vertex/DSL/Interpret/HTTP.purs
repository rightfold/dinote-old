module NN.Client.Vertex.DSL.Interpret.HTTP
( runVertexDSL
, runVertexDSLF
) where

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Bus (BusRW)
import Control.Monad.Aff.Skull as Skull
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Free (foldFree)
import Network.HTTP.Affjax (AJAX)
import NN.File (FileID)
import NN.Prelude
import NN.Vertex (Vertex, VertexID)
import NN.Client.Vertex.HTTP (createEdge, createVertex, getVertex, updateVertex)
import NN.Client.Vertex.DSL (VertexDSL, VertexDSLF(..))

runVertexDSL
    :: ∀ eff
     . BusRW (Tuple VertexID Vertex)
    -> Skull.State (ajax :: AJAX, avar :: AVAR, ref :: REF | eff) (FileID × VertexID) (Maybe Vertex)
    -> VertexDSL
    ~> Aff (ajax :: AJAX, avar :: AVAR, ref :: REF | eff)
runVertexDSL b s = foldFree f
    where f :: VertexDSLF ~> Aff (ajax :: AJAX, avar :: AVAR, ref :: REF | eff)
          f = runVertexDSLF b s

runVertexDSLF
    :: ∀ eff
     . BusRW (Tuple VertexID Vertex)
    -> Skull.State (ajax :: AJAX, avar :: AVAR, ref :: REF | eff) (FileID × VertexID) (Maybe Vertex)
    -> VertexDSLF
    ~> Aff (ajax :: AJAX, avar :: AVAR, ref :: REF | eff)
runVertexDSLF _ s (GetVertex fileID vertexID a) = a <$> getVertex s fileID vertexID
runVertexDSLF b _ (VertexBus a) = pure $ a b
runVertexDSLF _ _ (CreateVertex fileID a) = a <$> createVertex fileID
runVertexDSLF _ _ (UpdateVertex fileID vertexID vertex a) =
    a <$ updateVertex fileID vertexID vertex
runVertexDSLF _ _ (CreateEdge fileID edge a) = a <$ createEdge fileID edge


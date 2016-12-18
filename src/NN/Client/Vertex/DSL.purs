module NN.Client.Vertex.DSL
( VertexDSL
, VertexDSLF(..)
, getVertex
, vertexBus
, createVertex
, updateVertex
, createEdge
) where

import Control.Monad.Aff.Bus (BusRW)
import Control.Monad.Free (Free, liftF)
import NN.File (FileID)
import NN.Prelude
import NN.Vertex (Vertex, VertexID)

type VertexDSL = Free VertexDSLF

data VertexDSLF a
    = GetVertex FileID VertexID (Maybe Vertex -> a)
    | VertexBus (BusRW (Tuple VertexID Vertex) -> a)
    | CreateVertex FileID (VertexID -> a)
    | UpdateVertex FileID VertexID Vertex a
    | CreateEdge FileID {parentID :: VertexID, childID :: VertexID} a

getVertex :: FileID -> VertexID -> VertexDSL (Maybe Vertex)
getVertex fileID vertexID = liftF $ GetVertex fileID vertexID id

vertexBus :: VertexDSL (BusRW (Tuple VertexID Vertex))
vertexBus = liftF $ VertexBus id

createVertex :: FileID -> VertexDSL VertexID
createVertex fileID = liftF $ CreateVertex fileID id

updateVertex :: FileID -> VertexID -> Vertex -> VertexDSL Unit
updateVertex fileID vertexID vertex = liftF $ UpdateVertex fileID vertexID vertex unit

createEdge :: FileID -> {parentID :: VertexID, childID :: VertexID} -> VertexDSL Unit
createEdge fileID edge = liftF $ CreateEdge fileID edge unit

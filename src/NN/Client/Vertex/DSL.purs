module NN.Client.Vertex.DSL
( VertexDSL
, VertexDSLF(..)
, getVertex
, vertexBus
, createVertex
, createEdge
) where

import Control.Monad.Aff.Bus (BusRW)
import Control.Monad.Free (Free, liftF)
import NN.File (FileID)
import NN.Prelude
import NN.Vertex (Vertex, VertexID)

type VertexDSL = Free VertexDSLF

data VertexDSLF a
    = GetVertex VertexID (Maybe Vertex -> a)
    | VertexBus (BusRW (Tuple VertexID Vertex) -> a)
    | CreateVertex FileID (VertexID -> a)
    | CreateEdge {parentID :: VertexID, childID :: VertexID} a

getVertex :: VertexID -> VertexDSL (Maybe Vertex)
getVertex vertexID = liftF $ GetVertex vertexID id

vertexBus :: VertexDSL (BusRW (Tuple VertexID Vertex))
vertexBus = liftF $ VertexBus id

createVertex :: FileID -> VertexDSL VertexID
createVertex fileID = liftF $ CreateVertex fileID id

createEdge :: {parentID :: VertexID, childID :: VertexID} -> VertexDSL Unit
createEdge edge = liftF $ CreateEdge edge unit

module NN.Vertex.DSL
( VertexDSL
, VertexDSLF(..)
, getVertex
, vertexBus
) where

import Control.Monad.Aff.Bus (BusRW)
import NN.Prelude
import NN.Vertex (Vertex, VertexID)

type VertexDSL = Free VertexDSLF

data VertexDSLF a
    = GetVertex VertexID (Maybe Vertex -> a)
    | VertexBus VertexID (BusRW Vertex -> a)

getVertex :: VertexID -> VertexDSL (Maybe Vertex)
getVertex vertexID = liftF $ GetVertex vertexID id

vertexBus :: VertexID -> VertexDSL (BusRW Vertex)
vertexBus vertexID = liftF $ VertexBus vertexID id

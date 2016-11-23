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
    | VertexBus (BusRW (Tuple VertexID Vertex) -> a)

getVertex :: VertexID -> VertexDSL (Maybe Vertex)
getVertex vertexID = liftF $ GetVertex vertexID id

vertexBus :: VertexDSL (BusRW (Tuple VertexID Vertex))
vertexBus = liftF $ VertexBus id

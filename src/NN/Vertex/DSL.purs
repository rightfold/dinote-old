module NN.Vertex.DSL
( VertexDSL
, VertexDSLF(..)
) where

import Control.Monad.Aff.Bus (BusRW)
import Control.Monad.Free (Free)
import NN.Prelude
import NN.Vertex (Vertex, VertexID)

type VertexDSL = Free VertexDSLF

data VertexDSLF a
    = Get VertexID (Maybe Vertex -> a)
    | Bus VertexID (BusRW Vertex -> a)

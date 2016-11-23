module NN.DSL
( NNDSL
, NNDSLF
) where

import NN.Prelude
import NN.Vertex.DSL (VertexDSLF)

type NNDSL = Free NNDSLF

type NNDSLF = VertexDSLF

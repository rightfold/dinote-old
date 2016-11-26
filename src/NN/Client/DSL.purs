module NN.Client.DSL
( NNDSL
, NNDSLF
) where

import NN.Client.Vertex.DSL (VertexDSLF)
import NN.Prelude

type NNDSL = Free NNDSLF

type NNDSLF = VertexDSLF

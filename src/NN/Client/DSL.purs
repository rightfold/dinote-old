module NN.Client.DSL
( NNDSL
, NNDSLF
) where

import Control.Monad.Free (Free)
import NN.Client.Vertex.DSL (VertexDSLF)

type NNDSL = Free NNDSLF

type NNDSLF = VertexDSLF

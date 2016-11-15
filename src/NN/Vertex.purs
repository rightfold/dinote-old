module NN.Vertex
( VertexID
, Vertex
) where

import NN.Note (Note)
import NN.Prelude

newtype VertexID = VertexID String

derive instance eqVertexID :: Eq VertexID
derive instance ordVertexID :: Ord VertexID

type Vertex =
    { note :: Note
    , children :: List VertexID
    }

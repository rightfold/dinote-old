module NN.Vertex
( VertexID(..)
, Vertex
) where

import NN.Prelude
import NN.Vertex.Note (Note)
import NN.Vertex.Style (Style)

newtype VertexID = VertexID String

derive instance eqVertexID :: Eq VertexID
derive instance ordVertexID :: Ord VertexID

type Vertex =
    { note :: Note
    , children :: List VertexID
    , style :: Style
    }

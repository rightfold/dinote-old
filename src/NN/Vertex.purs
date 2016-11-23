module NN.Vertex
( VertexID(..)
, Vertex
) where

import NN.Prelude
import NN.Vertex.Style (Style)

newtype VertexID = VertexID String

derive instance eqVertexID :: Eq VertexID
derive instance ordVertexID :: Ord VertexID

instance showVertexID :: Show VertexID where
    show (VertexID v) = "(VertexID " <> show v <> ")"

type Vertex =
    { note :: String
    , children :: List VertexID
    , style :: Style
    }

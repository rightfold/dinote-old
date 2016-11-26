module NN.Vertex
( VertexID(..)
, Vertex(..)
) where

import Data.Sexp (class AsSexp, gFromSexp, gToSexp, class FromSexp, class ToSexp)
import NN.Prelude
import NN.Vertex.Style (Style)

newtype VertexID = VertexID String

derive instance genericVertexID :: Generic VertexID
derive instance eqVertexID :: Eq VertexID
derive instance ordVertexID :: Ord VertexID
instance showVertexID :: Show VertexID where show = gShow

data Vertex = Vertex String (List VertexID) Style

derive instance genericVertex :: Generic Vertex
instance showVertex :: Show Vertex where show = gShow
instance fromSexpVertex :: FromSexp Vertex where fromSexp = gFromSexp
instance toSexpVertex :: ToSexp Vertex where toSexp = gToSexp
instance asSexpVertex :: AsSexp Vertex

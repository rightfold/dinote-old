module NN.Vertex
( VertexID(..)
, Vertex(..)
, vertexNote
, vertexChildren
, vertexStyle
) where

import Data.Lens (Lens', lens)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Sexp (class AsSexp, genericFromSexp, genericToSexp, class FromSexp, class ToSexp)
import NN.Prelude
import NN.Vertex.Style (Style)

newtype VertexID = VertexID String

derive instance genericVertexID :: Generic VertexID _
derive instance eqVertexID :: Eq VertexID
derive instance ordVertexID :: Ord VertexID
instance fromSexpVertexID :: FromSexp VertexID where fromSexp = genericFromSexp
instance toSexpVertexID :: ToSexp VertexID where toSexp = genericToSexp
instance asSexpVertexID :: AsSexp VertexID
instance showVertexID :: Show VertexID where show = genericShow

data Vertex = Vertex String (List VertexID) Style

derive instance genericVertex :: Generic Vertex _
instance fromSexpVertex :: FromSexp Vertex where fromSexp = genericFromSexp
instance toSexpVertex :: ToSexp Vertex where toSexp = genericToSexp
instance asSexpVertex :: AsSexp Vertex
instance showVertex :: Show Vertex where show = genericShow

vertexNote :: Lens' Vertex String
vertexNote = lens (\(Vertex a _ _) -> a) (\(Vertex _ b c) a -> Vertex a b c)

vertexChildren :: Lens' Vertex (List VertexID)
vertexChildren = lens (\(Vertex _ b _) -> b) (\(Vertex a _ c) b -> Vertex a b c)

vertexStyle :: Lens' Vertex Style
vertexStyle = lens (\(Vertex _ _ c) -> c) (\(Vertex a b _) c -> Vertex a b c)

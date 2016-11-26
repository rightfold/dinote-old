module NN.Client.Vertex.HTTP
( fetchVertex
) where

import Data.Sexp as Sexp
import Network.HTTP.Affjax (AJAX)
import Network.HTTP.Affjax as Affjax
import NN.Prelude
import NN.Vertex (Vertex, VertexID(..))

fetchVertex :: ∀ eff. VertexID -> Aff (ajax :: AJAX | eff) (Maybe Vertex)
fetchVertex (VertexID vertexID) = do
    {response} <- Affjax.get ("http://localhost:1337/api/v1/vertices?vertexID=" <> vertexID)
    pure $ Sexp.fromString response >>= Sexp.fromSexp

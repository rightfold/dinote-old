module NN.Client.Vertex.HTTP
( fetchVertex
, createVertex
, appendChildVertex
) where

import Control.Monad.Eff.Exception (throw)
import Data.Sexp as Sexp
import Network.HTTP.Affjax (AJAX)
import Network.HTTP.Affjax as Affjax
import NN.Prelude
import NN.Vertex (Vertex, VertexID(..))

fetchVertex :: ∀ eff. VertexID -> Aff (ajax :: AJAX | eff) (Maybe Vertex)
fetchVertex (VertexID vertexID) = do
    {response} <- Affjax.get ("http://localhost:1337/api/v1/vertices/" <> vertexID)
    pure $ Sexp.fromString response >>= Sexp.fromSexp

createVertex
    :: ∀ eff
     . Aff (ajax :: AJAX | eff) VertexID
createVertex = do
    {response} <- Affjax.post ("http://localhost:1337/api/v1/vertices") unit
    case Sexp.fromString response >>= Sexp.fromSexp of
        Just vertexID -> pure vertexID
        Nothing -> liftEff' $ throw "could not create vertex"

appendChildVertex
    :: ∀ eff
     . {parentID :: VertexID, childID :: VertexID}
    -> Aff (ajax :: AJAX | eff) Unit
appendChildVertex {parentID, childID} =
    -- TODO
    pure unit

module NN.Client.Vertex.HTTP
( getVertex
, createVertex
, updateVertex
, createEdge
) where

import Control.Monad.Eff.Exception (throw)
import Data.Sexp as Sexp
import Network.HTTP.Affjax (AJAX)
import Network.HTTP.Affjax as Affjax
import NN.File (FileID(..))
import NN.Prelude
import NN.Vertex (Vertex, VertexID(..))

getVertex :: ∀ eff. FileID -> VertexID -> Aff (ajax :: AJAX | eff) (Maybe Vertex)
getVertex (FileID fileID) (VertexID vertexID) =
    Affjax.get ("/api/v1/files/" <> fileID <> "/vertices/" <> vertexID)
    <#> (_.response >>> Sexp.fromString >=> Sexp.fromSexp)

createVertex
    :: ∀ eff
     . FileID
    -> Aff (ajax :: AJAX | eff) VertexID
createVertex (FileID fileID) = do
    {response} <- Affjax.post ("/api/v1/files/" <> fileID <> "/vertices") unit
    case Sexp.fromString response >>= Sexp.fromSexp of
        Just vertexID -> pure vertexID
        Nothing -> liftEff' $ throw "could not create vertex"

updateVertex
    :: ∀ eff
     . FileID
    -> VertexID
    -> Vertex
    -> Aff (ajax :: AJAX | eff) Unit
updateVertex (FileID fileID) (VertexID vertexID) vertex =
    Affjax.put ("/api/v1/files/" <> fileID <> "/vertices/" <> vertexID)
               (Sexp.toString (Sexp.toSexp vertex))
    <#> _.response

createEdge
    :: ∀ eff
     . FileID
    -> {parentID :: VertexID, childID :: VertexID}
    -> Aff (ajax :: AJAX | eff) Unit
createEdge (FileID fileID) {parentID: VertexID parentID, childID: VertexID childID} =
    Affjax.post ("/api/v1/files/" <> fileID <> "/vertices/" <> parentID <> "/children/" <> childID) unit
    <#> _.response

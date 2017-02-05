module NN.Client.Vertex.HTTP
( getVertexBatcher
, getVertex
, createVertex
, updateVertex
, createEdge
) where

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Skull as Skull
import Control.Monad.Eff.Exception (throw)
import Control.Monad.Eff.Ref (REF)
import Data.Array as Array
import Data.Sexp as Sexp
import Data.String as String
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple as Tuple
import Network.HTTP.Affjax (AJAX)
import Network.HTTP.Affjax as Affjax
import NN.File (FileID(..))
import NN.Prelude
import NN.Vertex (Vertex, VertexID(..))

getVertexBatcher
    :: ∀ eff
     . Skull.Batcher (ajax :: AJAX | eff)
                     (FileID × VertexID)
                     (Maybe Vertex)
                     (List (FileID × VertexID))
                     (Maybe (List ((FileID × VertexID) × Vertex)))
                     (FileID × VertexID)
getVertexBatcher =
    { emptyBatch:    Nil
    , maxBatchDelay: Just (Milliseconds 10.0)
    , addRequest:    \r b -> (r : b) /\ r
    , getResponse:   \k b -> Tuple.lookup k =<< b
    , executeBatch
    }
    where
    executeBatch ids =
        Affjax.get ("/api/v1/files/batch/" <> args ids)
        <#> (_.response >>> Sexp.fromString >=> Sexp.fromSexp)
    args = Array.fromFoldable
           >>> map (\(FileID f /\ VertexID v) -> f <> "_" <> v)
           >>> String.joinWith "/"

getVertex
    :: ∀ eff
     . Skull.State (ajax :: AJAX, avar :: AVAR, ref :: REF | eff) (FileID × VertexID) (Maybe Vertex)
    -> FileID
    -> VertexID
    -> Aff (ajax :: AJAX, avar :: AVAR, ref :: REF | eff) (Maybe Vertex)
getVertex skull fileID vertexID = Skull.request skull (fileID /\ vertexID)

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

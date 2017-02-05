module NN.Server.Vertex.DSL.Interpret.DB
( runVertexDSL
, runVertexDSLF
) where

import Control.Monad.Free (foldFree)
import Database.PostgreSQL (Connection, POSTGRESQL)
import Data.UUID (GENUUID)
import NN.Prelude
import NN.Server.Vertex.DB as DB
import NN.Server.Vertex.DSL (VertexDSL, VertexDSLF(..))

runVertexDSL
    :: ∀ eff
     . Connection
    -> VertexDSL
    ~> Aff (uuid :: GENUUID, postgreSQL :: POSTGRESQL | eff)
runVertexDSL conn = foldFree (runVertexDSLF conn)

runVertexDSLF
    :: ∀ eff
     . Connection
    -> VertexDSLF
    ~> Aff (uuid :: GENUUID, postgreSQL :: POSTGRESQL | eff)
runVertexDSLF conn (GetVertex fileID vertexID next) = next <$> DB.readVertex conn fileID vertexID
runVertexDSLF conn (GetVertices ids next) =
    next <<< justs <$> for ids \(f /\ v) -> ((f /\ v) /\ _) <$> DB.readVertex conn f v
    where justs Nil = Nil
          justs ((_ /\ Nothing) : xs) = justs xs
          justs ((k /\ Just v) : xs) = (k /\ v) : justs xs
runVertexDSLF conn (CreateVertex fileID next) = next <$> DB.createVertex conn fileID
runVertexDSLF conn (UpdateVertex fileID vertexID vertex next) = next <$ DB.updateVertex conn fileID vertexID vertex
runVertexDSLF conn (CreateEdge fileID edge next) = next <$ DB.createEdge conn fileID edge

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
runVertexDSLF conn (GetVertex fileID vertexID next) = next <$> DB.readVertex conn vertexID
runVertexDSLF conn (CreateVertex fileID next) = next <$> DB.createVertex conn fileID
runVertexDSLF conn (UpdateVertex fileID vertexID vertex next) = next <$ DB.updateVertex conn fileID vertexID vertex
runVertexDSLF conn (CreateEdge fileID edge next) = next <$ DB.createEdge conn edge

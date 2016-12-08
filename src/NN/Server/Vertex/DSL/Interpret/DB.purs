module NN.Server.Vertex.DSL.Interpret.DB
( runVertexDSL
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
runVertexDSL conn = foldFree go
    where
    go :: VertexDSLF ~> Aff (uuid :: GENUUID, postgreSQL :: POSTGRESQL | eff)
    go (GetVertex fileID vertexID next) = next <$> DB.readVertex conn vertexID
    go (CreateVertex fileID next) = next <$> DB.createVertex conn fileID
    go (CreateEdge fileID edge next) = next <$ DB.createEdge conn edge

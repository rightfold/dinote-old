module NN.Server.Vertex.DSL.Interpret.Authorization
( runVertexDSLF
) where

import NN.Prelude
import NN.Server.Authorization.DSL (AuthorizationDSL, verifyAuthorizedForFile)
import NN.Server.Vertex.DSL (VertexDSLF(..))

runVertexDSLF :: ∀ a. VertexDSLF a -> AuthorizationDSL Unit
runVertexDSLF (GetVertex fileID _ _) = verifyAuthorizedForFile fileID
runVertexDSLF (CreateVertex fileID _) = verifyAuthorizedForFile fileID
runVertexDSLF (UpdateVertex fileID _ _ _) = verifyAuthorizedForFile fileID
runVertexDSLF (CreateEdge fileID _ _) = verifyAuthorizedForFile fileID

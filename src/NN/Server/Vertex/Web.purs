module NN.Server.Vertex.Web
( handleGetVertex
, handleCreateVertex
, handleCreateEdge
) where

import Network.HTTP.Message (Request, Response)
import NN.File (FileID)
import NN.Prelude
import NN.Server.Vertex.DSL (createEdge, createVertex, getVertex, VertexDSL)
import NN.Server.Web as Web
import NN.Vertex (VertexID)

handleGetVertex :: FileID -> VertexID -> Request -> VertexDSL Response
handleGetVertex fileID vertexID _ =
    getVertex fileID vertexID <#> maybe Web.notFound Web.ok

handleCreateVertex :: FileID -> Request -> VertexDSL Response
handleCreateVertex fileID _ = createVertex fileID <#> Web.ok

handleCreateEdge :: FileID -> {parentID :: VertexID, childID :: VertexID} -> Request -> VertexDSL Response
handleCreateEdge fileID edge _ = createEdge fileID edge $> Web.ok unit

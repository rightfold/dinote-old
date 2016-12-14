module NN.Server.Vertex.Web
( handleVertexAPI
, handleGetVertex
, handleCreateVertex
, handleCreateEdge
) where

import Network.HTTP.Message (Request, Response)
import NN.File (FileID)
import NN.Prelude
import NN.Server.Vertex.DSL (createEdge, createVertex, getVertex, VertexDSL)
import NN.Server.Web as Web
import NN.Vertex (VertexID(..))

handleVertexAPI :: FileID -> String -> List String -> Request -> VertexDSL Response
handleVertexAPI fileID method path req = case method, path of
    "GET", vertexID : Nil -> handleGetVertex fileID (VertexID vertexID) req
    "POST", Nil -> handleCreateVertex fileID req
    "POST", parentID : "children" : childID : Nil ->
        let edge = {parentID: VertexID parentID, childID: VertexID childID}
        in handleCreateEdge fileID edge req
    _, _ -> pure Web.notFound

handleGetVertex :: FileID -> VertexID -> Request -> VertexDSL Response
handleGetVertex fileID vertexID _ =
    getVertex fileID vertexID <#> maybe Web.notFound Web.ok

handleCreateVertex :: FileID -> Request -> VertexDSL Response
handleCreateVertex fileID _ = createVertex fileID <#> Web.ok

handleCreateEdge :: FileID -> {parentID :: VertexID, childID :: VertexID} -> Request -> VertexDSL Response
handleCreateEdge fileID edge _ = createEdge fileID edge $> Web.ok unit

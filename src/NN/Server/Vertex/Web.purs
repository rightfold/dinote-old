module NN.Server.Vertex.Web
( handleVertexAPI
, handleVertexBatchAPI
, handleGetVertex
, handleCreateVertex
, handleCreateEdge
) where

import Data.String as String
import Network.HTTP.Message (Request, Response)
import NN.File (FileID(..))
import NN.Prelude
import NN.Server.Vertex.DSL (createEdge, createVertex, getVertex, getVertices, updateVertex, VertexDSL)
import NN.Server.Web as Web
import NN.Vertex (VertexID(..))

handleVertexAPI :: FileID -> String -> List String -> Request -> VertexDSL Response
handleVertexAPI fileID method path req = case method, path of
    "GET", vertexID : Nil -> handleGetVertex fileID (VertexID vertexID) req
    "POST", Nil -> handleCreateVertex fileID req
    "PUT", vertexID : Nil -> handleUpdateVertex fileID (VertexID vertexID) req
    "POST", parentID : "children" : childID : Nil ->
        let edge = {parentID: VertexID parentID, childID: VertexID childID}
        in handleCreateEdge fileID edge req
    _, _ -> pure Web.notFound

handleVertexBatchAPI :: String -> List String -> Request -> VertexDSL Response
handleVertexBatchAPI method path req = case method, path of
    "GET", pairs ->
        let pair p = case String.split (String.Pattern "_") p of
                         [fileID, vertexID] -> Just (FileID fileID /\ VertexID vertexID)
                         _ -> Nothing
        in case traverse pair pairs of
               Just pairs' -> handleGetVertices pairs' req
               Nothing     -> pure Web.notFound
    _, _ -> pure Web.notFound

handleGetVertex :: FileID -> VertexID -> Request -> VertexDSL Response
handleGetVertex fileID vertexID _ =
    getVertex fileID vertexID <#> maybe Web.notFound Web.ok

handleGetVertices :: List (FileID × VertexID) -> Request -> VertexDSL Response
handleGetVertices ids _ =
    getVertices ids <#> Web.ok

handleCreateVertex :: FileID -> Request -> VertexDSL Response
handleCreateVertex fileID _ = createVertex fileID <#> Web.ok

handleUpdateVertex :: FileID -> VertexID -> Request -> VertexDSL Response
handleUpdateVertex fileID vertexID = Web.interact \vertex ->
    updateVertex fileID vertexID vertex $> Web.ok unit

handleCreateEdge :: FileID -> {parentID :: VertexID, childID :: VertexID} -> Request -> VertexDSL Response
handleCreateEdge fileID edge _ = createEdge fileID edge $> Web.ok unit

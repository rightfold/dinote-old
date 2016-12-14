module NN.Server.Vertex.Web
( handleGetVertex
, handleCreateVertex
, handleCreateEdge
) where

import Data.ByteString as ByteString
import Data.Map (Map)
import Data.Map as Map
import Data.Sexp as Sexp
import Data.String.CaseInsensitive (CaseInsensitiveString)
import Network.HTTP.Message (Request, Response)
import Node.Encoding (Encoding(UTF8))
import NN.File (FileID)
import NN.Prelude
import NN.Server.Vertex.DSL (createEdge, createVertex, getVertex, VertexDSL)
import NN.Vertex (VertexID)

handleGetVertex :: FileID -> VertexID -> Request -> VertexDSL Response
handleGetVertex fileID vertexID _ =
    getVertex fileID vertexID <#> case _ of
        Just vertex ->
            { status: {code: 200, message: "OK"}
            , headers: Map.empty :: Map CaseInsensitiveString String
            , body: ByteString.fromString (Sexp.toString $ Sexp.toSexp vertex) UTF8
            }
        Nothing ->
            { status: {code: 404, message: "Not Found"}
            , headers: Map.empty :: Map CaseInsensitiveString String
            , body: ByteString.empty
            }

handleCreateVertex :: FileID -> Request -> VertexDSL Response
handleCreateVertex fileID _ =
    createVertex fileID <#> \vertexID ->
        { status: {code: 200, message: "OK"}
        , headers: Map.empty :: Map CaseInsensitiveString String
        , body: ByteString.fromString (Sexp.toString $ Sexp.toSexp vertexID) UTF8
        }

handleCreateEdge :: FileID -> {parentID :: VertexID, childID :: VertexID} -> Request -> VertexDSL Response
handleCreateEdge fileID edge _ =
    createEdge fileID edge $>
        { status: {code: 200, message: "OK"}
        , headers: Map.empty :: Map CaseInsensitiveString String
        , body: ByteString.empty
        }

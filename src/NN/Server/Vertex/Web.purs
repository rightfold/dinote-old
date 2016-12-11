module NN.Server.Vertex.Web
( handleCreateVertex
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
import NN.Server.Vertex.DSL (createVertex, VertexDSL)

handleCreateVertex :: FileID -> Request -> VertexDSL Response
handleCreateVertex fileID _ =
    createVertex fileID <#> \vertexID ->
        { status: {code: 200, message: "OK"}
        , headers: Map.empty :: Map CaseInsensitiveString String
        , body: ByteString.fromString (Sexp.toString $ Sexp.toSexp vertexID) UTF8
        }

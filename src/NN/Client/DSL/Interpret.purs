module NN.Client.DSL.Interpret
( runAuthenticationDSL
, runAuthenticationDSLF
, runVertexDSL
, runVertexDSLF
) where

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Bus (BusRW)
import Control.Monad.Free (foldFree)
import Network.HTTP.Affjax (AJAX)
import NN.Prelude
import NN.Vertex (Vertex, VertexID)
import NN.Client.Authentication.DSL (AuthenticationDSL, AuthenticationDSLF(..))
import NN.Client.Vertex.HTTP (createEdge, createVertex, getVertex)
import NN.Client.Vertex.DSL (VertexDSL, VertexDSLF(..))

runAuthenticationDSL
    :: ∀ eff
     . AuthenticationDSL
    ~> Aff eff
runAuthenticationDSL = foldFree runAuthenticationDSLF

runAuthenticationDSLF
    :: ∀ eff
     . AuthenticationDSLF
    ~> Aff eff
runAuthenticationDSLF (Authenticate username password next) = pure $ next Nothing

runVertexDSL
    :: ∀ eff
     . BusRW (Tuple VertexID Vertex)
    -> VertexDSL
    ~> Aff (ajax :: AJAX, avar :: AVAR | eff)
runVertexDSL = foldFree <<< runVertexDSLF

runVertexDSLF
    :: ∀ eff
     . BusRW (Tuple VertexID Vertex)
    -> VertexDSLF
    ~> Aff (ajax :: AJAX, avar :: AVAR | eff)
runVertexDSLF _ (GetVertex fileID vertexID a) = a <$> getVertex fileID vertexID
runVertexDSLF vertexBus (VertexBus a) = pure $ a vertexBus
runVertexDSLF _ (CreateVertex fileID a) = a <$> createVertex fileID
runVertexDSLF _ (CreateEdge fileID edge a) = a <$ createEdge fileID edge

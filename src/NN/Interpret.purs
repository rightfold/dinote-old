module NN.Interpret
( interpret
, runNNDSL
, runVertexDSL
) where

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Bus (BusRW)
import Data.Argonaut.Core as JSON
import Data.List as List
import Data.StrMap as StrMap
import Network.HTTP.Affjax (AJAX)
import Network.HTTP.Affjax as Affjax
import NN.DSL (NNDSL)
import NN.Prelude
import NN.Vertex (Vertex, VertexID(..))
import NN.Vertex.DSL (VertexDSL, VertexDSLF(..))
import NN.Vertex.Style (Style(..))

interpret
    :: ∀ eff
     . BusRW (Tuple VertexID Vertex)
    -> Free (Aff (ajax :: AJAX, avar :: AVAR | eff) ⊕ NNDSL)
    ~> Aff (ajax :: AJAX, avar :: AVAR | eff)
interpret vertexBus = foldFree (coproduct id (runNNDSL vertexBus))

runNNDSL
    :: ∀ eff
     . BusRW (Tuple VertexID Vertex)
    -> NNDSL
    ~> Aff (ajax :: AJAX, avar :: AVAR | eff)
runNNDSL = runVertexDSL

runVertexDSL
    :: ∀ eff
     . BusRW (Tuple VertexID Vertex)
    -> VertexDSL
    ~> Aff (ajax :: AJAX, avar :: AVAR | eff)
runVertexDSL vertexBus = foldFree go
    where
    go :: VertexDSLF ~> Aff (ajax :: AJAX, avar :: AVAR | eff)
    go (GetVertex (VertexID vertexID) a) = do
        {response} <- Affjax.get ("/api/v1/vertices?vertexID=" <> vertexID)
        pure $ a $
            {note: _, children: _, style: Normal}
                <$> (response # JSON.toObject >>= StrMap.lookup "note" >>= JSON.toString)
                <*> (response # JSON.toObject >>= StrMap.lookup "children" >>= JSON.toArray >>= traverse JSON.toString <#> map VertexID <#> List.fromFoldable)
    go (VertexBus a) = pure $ a vertexBus

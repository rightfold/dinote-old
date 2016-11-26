module NN.Client.Vertex.HTTP
( fetchVertex
) where

import Data.Argonaut.Core as JSON
import Data.List as List
import Data.StrMap as StrMap
import Network.HTTP.Affjax (AJAX)
import Network.HTTP.Affjax as Affjax
import NN.Prelude
import NN.Vertex (Vertex, VertexID(..))
import NN.Vertex.Style (Style(..))

fetchVertex :: ∀ eff. VertexID -> Aff (ajax :: AJAX | eff) (Maybe Vertex)
fetchVertex (VertexID vertexID) = do
    {response} <- Affjax.get ("/api/v1/vertices?vertexID=" <> vertexID)
    pure $
        {note: _, children: _, style: Normal}
            <$> (response # JSON.toObject >>= StrMap.lookup "note" >>= JSON.toString)
            <*> (response # JSON.toObject >>= StrMap.lookup "children" >>= JSON.toArray >>= traverse JSON.toString <#> map VertexID <#> List.fromFoldable)

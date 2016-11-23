module NN.Interpret
( interpret
, runNNDSL
, runVertexDSL
) where

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Bus (BusRW)
import Control.Monad.Aff.Bus as Bus
import Control.Monad.Eff.Ref (readRef, REF, Ref, writeRef)
import Data.Argonaut.Core as JSON
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
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
     . Ref (Map VertexID (BusRW Vertex))
    -> Free (Aff (ajax :: AJAX, avar :: AVAR, ref :: REF | eff) ⊕ NNDSL)
    ~> Aff (ajax :: AJAX, avar :: AVAR, ref :: REF | eff)
interpret busesRef = foldFree (coproduct id (runNNDSL busesRef))

runNNDSL
    :: ∀ eff
     . Ref (Map VertexID (BusRW Vertex))
    -> NNDSL
    ~> Aff (ajax :: AJAX, avar :: AVAR, ref :: REF | eff)
runNNDSL = runVertexDSL

runVertexDSL
    :: ∀ eff
     . Ref (Map VertexID (BusRW Vertex))
    -> VertexDSL
    ~> Aff (ajax :: AJAX, avar :: AVAR, ref :: REF | eff)
runVertexDSL busesRef = foldFree go
    where
    go :: VertexDSLF ~> Aff (ajax :: AJAX, avar :: AVAR, ref :: REF | eff)
    go (GetVertex (VertexID vertexID) a) = do
        {response} <- Affjax.get ("/api/v1/vertices?vertexID=" <> vertexID)
        pure $ a $
            {note: _, children: _, style: Normal}
                <$> (response # JSON.toObject >>= StrMap.lookup "note" >>= JSON.toString)
                <*> (response # JSON.toObject >>= StrMap.lookup "children" >>= JSON.toArray >>= traverse JSON.toString <#> map VertexID <#> List.fromFoldable)
    go (VertexBus vertexID a) = do
        freshBus <- Bus.make
        bus <- liftEff $ do
            buses <- readRef busesRef
            case Map.lookup vertexID buses of
                Nothing -> do
                    writeRef busesRef $ Map.insert vertexID freshBus buses
                    pure freshBus
                Just bus -> pure bus
        pure $ a bus

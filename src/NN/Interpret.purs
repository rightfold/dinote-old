module NN.Interpret
( interpret
, runNNDSL
, runVertexDSL
) where

import Control.Monad.Aff.Bus (BusRW)
import Control.Monad.Aff.Bus as Bus
import Control.Monad.Eff.Ref (readRef, REF, Ref, writeRef)
import Data.Map (Map)
import Data.Map as Map
import NN.DSL (NNDSL)
import NN.Prelude
import NN.Vertex (Vertex, VertexID(..))
import NN.Vertex.DSL (VertexDSL, VertexDSLF(..))
import NN.Vertex.Note (Note(..))
import NN.Vertex.Style (Style(..))

interpret
    :: ∀ eff
     . Ref (Map VertexID (BusRW Vertex))
    -> Free (Aff (avar :: AVAR, ref :: REF | eff) ⊕ NNDSL)
    ~> Aff (avar :: AVAR, ref :: REF | eff)
interpret busesRef = foldFree (coproduct id (runNNDSL busesRef))

runNNDSL
    :: ∀ eff
     . Ref (Map VertexID (BusRW Vertex))
    -> NNDSL
    ~> Aff (avar :: AVAR, ref :: REF | eff)
runNNDSL = runVertexDSL

runVertexDSL
    :: ∀ eff
     . Ref (Map VertexID (BusRW Vertex))
    -> VertexDSL
    ~> Aff (avar :: AVAR, ref :: REF | eff)
runVertexDSL busesRef = foldFree go
    where
    go :: VertexDSLF ~> Aff (avar :: AVAR, ref :: REF | eff)
    go (GetVertex vertexID a) = pure $ a $ case vertexID of
        VertexID "92eacb4c-a841-4b96-a984-a077caba347c" -> Just {note: Text "root", children: VertexID "9733d16e-d506-428a-a135-c3e7d886c396" : VertexID "78c48c7e-7fad-48a8-815f-9f4c2ce43fd7" : Nil, style: Normal}
        VertexID "9733d16e-d506-428a-a135-c3e7d886c396" -> Just {note: Text "a", children: VertexID "d2e7f648-e8a7-4891-8754-4543498a8f57" : Nil, style: Dimmed}
        VertexID "78c48c7e-7fad-48a8-815f-9f4c2ce43fd7" -> Just {note: Text "b", children: VertexID "d2e7f648-e8a7-4891-8754-4543498a8f57" : VertexID "92eacb4c-a841-4b96-a984-a077caba347c" : Nil, style: Peachpuff}
        VertexID "d2e7f648-e8a7-4891-8754-4543498a8f57" -> Just {note: Text "c", children: VertexID "0fe66fb3-c195-49d1-a31b-983d8191f6a5" : VertexID "5c9151bc-df1d-4ff0-87c4-60e66642b8a9" : Nil, style: HotDogStand}
        VertexID "0fe66fb3-c195-49d1-a31b-983d8191f6a5" -> Just {note: Text "d", children: Nil, style: Grass}
        VertexID "5c9151bc-df1d-4ff0-87c4-60e66642b8a9" -> Just {note: Text "e", children: Nil, style: Warning}
        _ -> Nothing
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

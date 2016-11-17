module NN.Interpret
( interpret
, runNNDSL
, runVertexDSL
) where

import Control.Monad.Aff.Bus as Bus
import NN.DSL (NNDSL)
import NN.Prelude
import NN.Vertex (VertexID(..))
import NN.Vertex.DSL (VertexDSL, VertexDSLF(..))
import NN.Vertex.Note (Note(..))
import NN.Vertex.Style (Style(..))

interpret
    :: ∀ eff
     . Free (Aff (avar :: AVAR | eff) ⊕ NNDSL)
    ~> Aff (avar :: AVAR | eff)
interpret = foldFree (coproduct id runNNDSL)

runNNDSL :: ∀ eff. NNDSL ~> Aff (avar :: AVAR | eff)
runNNDSL = runVertexDSL

runVertexDSL :: ∀ eff. VertexDSL ~> Aff (avar :: AVAR | eff)
runVertexDSL = foldFree go
    where
    go :: VertexDSLF ~> Aff (avar :: AVAR | eff)
    go (GetVertex vertexID a) = pure $ a $ case vertexID of
        VertexID "92eacb4c-a841-4b96-a984-a077caba347c" -> Just {note: Text "root", children: VertexID "9733d16e-d506-428a-a135-c3e7d886c396" : VertexID "78c48c7e-7fad-48a8-815f-9f4c2ce43fd7" : Nil, style: Normal}
        VertexID "9733d16e-d506-428a-a135-c3e7d886c396" -> Just {note: Text "a", children: VertexID "d2e7f648-e8a7-4891-8754-4543498a8f57" : Nil, style: Dimmed}
        VertexID "78c48c7e-7fad-48a8-815f-9f4c2ce43fd7" -> Just {note: Text "b", children: VertexID "d2e7f648-e8a7-4891-8754-4543498a8f57" : VertexID "92eacb4c-a841-4b96-a984-a077caba347c" : Nil, style: Salmon}
        VertexID "d2e7f648-e8a7-4891-8754-4543498a8f57" -> Just {note: Text "c", children: Nil, style: HotDogStand}
        _ -> Nothing
    go (VertexBus _ a) = a <$> Bus.make

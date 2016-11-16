module NN.Interpret
( interpret
) where

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Bus as Bus
import NN.DSL (NNDSLF)
import NN.Note (Note(..))
import NN.Prelude
import NN.Vertex (VertexID(..))
import NN.Vertex.DSL (VertexDSLF(..))

interpret
    :: ∀ eff
     . Free (Aff (avar :: AVAR | eff) ⊕ NNDSLF)
    ~> Aff (avar :: AVAR | eff)
interpret = foldFree (coproduct goAff goVertexDSLF)
    where
    goAff :: Aff (avar :: AVAR | eff) ~> Aff (avar :: AVAR | eff)
    goAff = id

    goVertexDSLF :: VertexDSLF ~> Aff (avar :: AVAR | eff)
    goVertexDSLF (GetVertex vertexID a) = pure $ a $ case vertexID of
        VertexID "92eacb4c-a841-4b96-a984-a077caba347c" -> Just {note: Text "root", children: VertexID "9733d16e-d506-428a-a135-c3e7d886c396" : VertexID "78c48c7e-7fad-48a8-815f-9f4c2ce43fd7" : Nil}
        VertexID "9733d16e-d506-428a-a135-c3e7d886c396" -> Just {note: Text "a", children: VertexID "d2e7f648-e8a7-4891-8754-4543498a8f57" : Nil}
        VertexID "78c48c7e-7fad-48a8-815f-9f4c2ce43fd7" -> Just {note: Text "b", children: VertexID "d2e7f648-e8a7-4891-8754-4543498a8f57" : VertexID "92eacb4c-a841-4b96-a984-a077caba347c" : Nil}
        VertexID "d2e7f648-e8a7-4891-8754-4543498a8f57" -> Just {note: Text "c", children: Nil}
        _ -> Nothing
    goVertexDSLF (VertexBus _ a) = a <$> Bus.make

module NN.Workspace
( Query
, Output
, Monad
, ui
) where

import Data.Set as Set
import Halogen.Component (Component, parentComponent, ParentDSL, ParentHTML)
import Halogen.HTML (HTML)
import Halogen.HTML as H
import NN.DSL (NNDSLF)
import NN.Prelude
import NN.Vertex (VertexID(..))
import NN.Vertex.UI as Vertex.UI

type State = Unit

newtype Query a = Query Void

type Output = Void

type Slot = Unit

type Monad eff = Free (Aff eff ⊕ NNDSLF)

ui :: ∀ eff. Component HTML Query Output (Monad eff)
ui = parentComponent {initialState, render, eval}
    where
    initialState :: State
    initialState = unit

    render :: State -> ParentHTML Query Vertex.UI.Query Slot (Monad eff)
    render _ =
        let rootID = VertexID "92eacb4c-a841-4b96-a984-a077caba347c"
        in H.slot unit (defer \_ -> Vertex.UI.ui rootID Set.empty) absurd

    eval :: Query ~> ParentDSL State Query Vertex.UI.Query Slot Output (Monad eff)
    eval (Query void) = absurd void

module NN.Workspace
( Query
, Output
, Monad
, ui
) where

import Control.Monad.Aff.Bus (BusRW)
import Control.Monad.Aff.Bus as Bus
import Control.Monad.State.Class as State
import Data.Set as Set
import Halogen.Component.Bus (busEvents)
import Halogen.HTML as H
import NN.DSL (NNDSL)
import NN.Prelude.Halogen
import NN.Vertex (VertexID(..))
import NN.Vertex.UI as Vertex.UI

type State =
    { selectedVertexBus :: BusRW (Maybe VertexID)
    , selectedVertexID :: Maybe VertexID
    }

data Query a
    = Initialize a
    | UpdateSelection (Maybe VertexID) a

type Output = Void

type Slot = Unit

type Monad eff = Free (Aff (avar :: AVAR | eff) ⊕ NNDSL)

mLiftAff :: ∀ eff. Aff (avar :: AVAR | eff) ~> Monad eff
mLiftAff = liftF <<< left

ui :: ∀ eff. Component HTML Query Output (Monad eff)
ui = lifecycleParentComponent {initialState, render, eval, initializer, finalizer}
    where
    initialState :: Maybe State
    initialState = Nothing

    render :: Maybe State -> ParentHTML Query Vertex.UI.Query Slot (Monad eff)
    render Nothing = H.div [] []
    render (Just s) = H.div [] [renderTree s, renderInspector s]

    renderTree :: State -> ParentHTML Query Vertex.UI.Query Slot (Monad eff)
    renderTree {selectedVertexBus} =
        H.section []
            let rootID = VertexID "92eacb4c-a841-4b96-a984-a077caba347c"
            in [H.slot unit (defer \_ -> Vertex.UI.ui rootID Set.empty selectedVertexBus) absurd]

    renderInspector :: State -> ParentHTML Query Vertex.UI.Query Slot (Monad eff)
    renderInspector {selectedVertexID} = H.text (show selectedVertexID)

    eval :: Query ~> ParentDSL (Maybe State) Query Vertex.UI.Query Slot Output (Monad eff)
    eval (Initialize next) = do
        selectedVertexBus <- lift (mLiftAff Bus.make)
        State.put $ Just {selectedVertexBus, selectedVertexID: Nothing}
        hoistM mLiftAff $ subscribe (busEvents selectedVertexBus (Just <<< action <<< UpdateSelection))
        pure next
    eval (UpdateSelection newSelectedVertexID next) =
        next <$ State.modify (_ <#> _ {selectedVertexID = newSelectedVertexID})

    initializer :: Maybe (Query Unit)
    initializer = Just $ action Initialize

    finalizer :: Maybe (Query Unit)
    finalizer = Nothing

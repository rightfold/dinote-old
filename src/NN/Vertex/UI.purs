module NN.Vertex.UI
( Query
, Output
, Monad
, ui
) where

import Control.Monad.Aff.Bus (BusRW)
import Control.Monad.Aff.Bus as Bus
import Control.Monad.State.Class as State
import Data.List as List
import Data.Set (Set)
import Data.Set as Set
import Halogen.Component.Bus (busEvents)
import Halogen.HTML as H
import Halogen.HTML.Events as E
import Halogen.HTML.Properties as P
import NN.Prelude.Halogen
import NN.Vertex (Vertex, VertexID)
import NN.Vertex.DSL (getVertex, vertexBus, VertexDSL)
import NN.Vertex.Note (Note(..))
import NN.Vertex.Style (styleClass)

type State =
    { vertex :: Maybe Vertex
    , selectedVertexID :: Maybe VertexID
    }

data Query a
    = Initialize a
    | UpdateVertex Vertex a
    | ToggleSelfSelection a
    | UpdateSelection (Maybe VertexID) a

type Output = Void

type Slot = Int

type Monad eff = Free (Aff (avar :: AVAR | eff) ⊕ VertexDSL)

mLiftAff :: ∀ eff. Aff (avar :: AVAR | eff) ~> Monad eff
mLiftAff = liftF <<< left

mLiftVertexDSL :: ∀ eff. VertexDSL ~> Monad eff
mLiftVertexDSL = liftF <<< right

ui :: ∀ eff. VertexID -> Set VertexID -> BusRW (Maybe VertexID) -> Component HTML Query Output (Monad eff)
ui vertexID parentIDs selectedVertexBus =
    lifecycleParentComponent {initialState, render, eval, initializer, finalizer}
    where
    initialState :: State
    initialState = {vertex: Nothing, selectedVertexID: Nothing}

    render :: State -> ParentHTML Query Query Slot (Monad eff)
    render state
        | Set.member vertexID parentIDs = renderCycleIndicator
        | otherwise = renderVertex state

    renderCycleIndicator :: ParentHTML Query Query Slot (Monad eff)
    renderCycleIndicator = H.text "(cycle)"

    renderVertex :: State -> ParentHTML Query Query Slot (Monad eff)
    renderVertex {vertex: Nothing} = H.text "(loading)"
    renderVertex {vertex: Just {note, children, style}, selectedVertexID} =
        H.article [P.classes ([styleClass style] <> if Just vertexID == selectedVertexID then [ClassName "-selected"] else [])]
            [ H.section [ P.class_ (ClassName "-note")
                        , E.onClick (E.input_ ToggleSelfSelection)
                        ] $
                renderNote note
            , H.section [P.class_ (ClassName "-children")]
                [ H.ul [] $
                    children
                    # List.mapWithIndex (flip renderChild)
                    # map (H.li [])
                    # List.toUnfoldable
                ]
            ]

    renderNote :: ∀ a. Note -> Array (HTML a (Query Unit))
    renderNote Empty = []
    renderNote (Append a b) = renderNote a <> renderNote b
    renderNote (Text text) = [H.text text]

    renderChild :: Slot -> VertexID -> Array (ParentHTML Query Query Slot (Monad eff))
    renderChild slot childID =
        let childComponent = defer \_ -> ui childID (Set.insert vertexID parentIDs) selectedVertexBus
        in [H.slot slot childComponent absurd]

    eval :: Query ~> ParentDSL State Query Query Slot Output (Monad eff)
    eval (Initialize next) = immediate *> subsequent *> selection $> next
        where
        immediate = do
            vertex <- lift (mLiftVertexDSL (getVertex vertexID))
            State.modify _ {vertex = vertex}
        subsequent = do
            bus <- lift $ mLiftVertexDSL (vertexBus vertexID)
            hoistM mLiftAff $ subscribe (busEvents bus (Just <<< action <<< UpdateVertex))
        selection = hoistM mLiftAff $ subscribe (busEvents selectedVertexBus (Just <<< action <<< UpdateSelection))
    eval (UpdateVertex vertex next) = next <$ State.modify _ {vertex = Just vertex}
    eval (ToggleSelfSelection next) = do
        selfSelected <- State.gets \{selectedVertexID} -> selectedVertexID == Just vertexID
        let newSelection = if selfSelected then Nothing else Just vertexID
        next <$ lift (mLiftAff (Bus.write newSelection selectedVertexBus))
    eval (UpdateSelection newSelectedVertexID next) = next <$ State.modify _ {selectedVertexID = newSelectedVertexID}

    initializer :: Maybe (Query Unit)
    initializer = Just $ action Initialize

    finalizer :: Maybe (Query Unit)
    finalizer = Nothing

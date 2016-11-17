module NN.Vertex.UI
( Query
, Output
, Monad
, ui
) where

import Control.Monad.State.Class as State
import Data.List as List
import Data.Set (Set)
import Data.Set as Set
import Halogen.Component.Bus (busEvents)
import Halogen.HTML as H
import Halogen.HTML.Properties as P
import NN.Prelude.Halogen
import NN.Vertex (Vertex, VertexID)
import NN.Vertex.DSL (getVertex, vertexBus, VertexDSL)
import NN.Vertex.Note (Note(..))
import NN.Vertex.Style (styleClass)

type State = Maybe Vertex

data Query a
    = Initialize a
    | Update Vertex a

type Output = Void

type Slot = Int

type Monad eff = Free (Aff (avar :: AVAR | eff) ⊕ VertexDSL)

mLiftAff :: ∀ eff. Aff (avar :: AVAR | eff) ~> Monad eff
mLiftAff = liftF <<< left

mLiftVertexDSL :: ∀ eff. VertexDSL ~> Monad eff
mLiftVertexDSL = liftF <<< right

ui :: ∀ eff. VertexID -> Set VertexID -> Component HTML Query Output (Monad eff)
ui vertexID parentIDs =
    lifecycleParentComponent {initialState, render, eval, initializer, finalizer}
    where
    initialState :: State
    initialState = Nothing

    render :: State -> ParentHTML Query Query Slot (Monad eff)
    render state
        | Set.member vertexID parentIDs = renderCycleIndicator
        | otherwise = renderVertex state

    renderCycleIndicator :: ParentHTML Query Query Slot (Monad eff)
    renderCycleIndicator = H.text "(cycle)"

    renderVertex :: State -> ParentHTML Query Query Slot (Monad eff)
    renderVertex Nothing = H.text "(loading)"
    renderVertex (Just {note, children, style}) =
        H.article [P.class_ (styleClass style)]
            [ H.section [P.class_ (ClassName "-note")] $ renderNote note
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
        let childComponent = defer \_ -> ui childID (Set.insert vertexID parentIDs)
        in [H.slot slot childComponent absurd]

    eval :: Query ~> ParentDSL State Query Query Slot Output (Monad eff)
    eval (Initialize next) = immediate *> subsequent $> next
        where
        immediate = lift (mLiftVertexDSL (getVertex vertexID)) >>= State.put
        subsequent = do
            bus <- lift $ mLiftVertexDSL (vertexBus vertexID)
            hoistM mLiftAff $ subscribe (busEvents bus (Just <<< action <<< Update))
    eval (Update vertex next) = next <$ State.put (Just vertex)

    initializer :: Maybe (Query Unit)
    initializer = Just $ action Initialize

    finalizer :: Maybe (Query Unit)
    finalizer = Nothing

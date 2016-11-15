module NN.Vertex.UI
( Query
, Output
, ui
) where

import Control.Monad.State.Class as State
import Control.Monad.Trans.Class (lift)
import Data.List as List
import Data.Set (Set)
import Data.Set as Set
import Halogen.Component (Component, lifecycleParentComponent, ParentDSL, ParentHTML)
import Halogen.HTML (HTML)
import Halogen.HTML as H
import Halogen.Query (action)
import NN.Note (Note(..))
import NN.Prelude
import NN.Vertex (Vertex, VertexID)
import NN.Vertex.DSL (getVertex, VertexDSL)

type State = Maybe Vertex

newtype Query a
    = Initialize a

type Output = Void

type Slot = Int

ui :: VertexID -> Set VertexID -> Component HTML Query Output VertexDSL
ui vertexID parentIDs =
    lifecycleParentComponent {initialState, render, eval, initializer, finalizer}
    where
    initialState :: State
    initialState = Nothing

    render :: State -> ParentHTML Query Query Slot VertexDSL
    render state
        | Set.member vertexID parentIDs = renderCycleIndicator
        | otherwise = renderVertex state

    renderCycleIndicator :: ParentHTML Query Query Slot VertexDSL
    renderCycleIndicator = H.text "(cycle)"

    renderVertex :: State -> ParentHTML Query Query Slot VertexDSL
    renderVertex Nothing = H.text "(loading)"
    renderVertex (Just {note, children}) =
        H.article []
            [ H.section [] $ renderNote note
            , H.section []
                [ H.ul [] $
                    children
                    # mapWithIndex renderChild
                    # map (H.li [])
                    # List.toUnfoldable
                ]
            ]

    renderNote :: ∀ a. Note -> Array (HTML a (Query Unit))
    renderNote Empty = []
    renderNote (Append a b) = renderNote a <> renderNote b
    renderNote (Text text) = [H.text text]

    renderChild :: Slot -> VertexID -> Array (ParentHTML Query Query Slot VertexDSL)
    renderChild slot childID =
        let childComponent = defer \_ -> ui childID (Set.insert vertexID parentIDs)
        in [H.slot slot childComponent absurd]

    eval :: Query ~> ParentDSL State Query Query Slot Output VertexDSL
    eval (Initialize next) = next <$ (State.put =<< lift (getVertex vertexID))

    initializer :: Maybe (Query Unit)
    initializer = Just $ action Initialize

    finalizer :: Maybe (Query Unit)
    finalizer = Nothing

mapWithIndex :: ∀ a b. (Int -> a -> b) -> List a -> List b
mapWithIndex f = foldr step {i: 0, acc: Nil} >>> _.acc
    where step x {i, acc} = {i: i + 1, acc: f i x : acc}

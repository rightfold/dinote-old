module NN.Vertex.UI
( Query
, Output
, Monad
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
import NN.Vertex.DSL (getVertex, VertexDSLF)

type State = Maybe Vertex

newtype Query a
    = Initialize a

type Output = Void

type Slot = Int

type Monad eff = Free (Aff eff ⊕ VertexDSLF)

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
    renderVertex (Just {note, children}) =
        H.article []
            [ H.section [] $ renderNote note
            , H.section []
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
    eval (Initialize next) = do
      State.put =<< lift (hoistFree right (getVertex vertexID))
      pure next

    initializer :: Maybe (Query Unit)
    initializer = Just $ action Initialize

    finalizer :: Maybe (Query Unit)
    finalizer = Nothing

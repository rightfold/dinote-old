module NN.Client.Vertex.UI
( Query
, Output
, Monad
, ui
) where

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Bus as Bus
import Control.Monad.Free (Free, liftF)
import Control.Monad.State.Class as State
import Control.Monad.Trans.Class (lift)
import Data.Functor.Coproduct (left, right)
import Data.Lazy (defer)
import Data.List as List
import Data.Set (Set)
import Data.Set as Set
import Halogen.Component.Bus (busEvents)
import Halogen.HTML as H
import Halogen.HTML.Events as E
import Halogen.HTML.Properties as P
import NN.Client.Vertex.DSL (getVertex, newVertex, vertexBus, VertexDSL)
import NN.Prelude.Halogen
import NN.Vertex (Vertex(..), VertexID)
import NN.Vertex.Style (Style(..))

type State = Maybe Vertex

data Query a
    = Initialize a
    | UpdateVertex Vertex a
    | EditNote String a
    | EditStyle Style a
    | AddNewVertex a

instance functorQuery :: Functor Query where
    map f (Initialize next) = Initialize (f next)
    map f (UpdateVertex vertex next) = UpdateVertex vertex (f next)
    map f (EditNote text next) = EditNote text (f next)
    map f (EditStyle style next) = EditStyle style (f next)
    map f (AddNewVertex next) = AddNewVertex (f next)

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
        | isCycle = renderCycleIndicator
        | otherwise = renderVertex state

    renderCycleIndicator :: ParentHTML Query Query Slot (Monad eff)
    renderCycleIndicator = H.text "(cycle)"

    renderVertex :: State -> ParentHTML Query Query Slot (Monad eff)
    renderVertex Nothing = H.text "(loading)"
    renderVertex (Just (Vertex note children style)) =
        H.article [P.classes [ClassName "nn--vertex", styleClass style]]
            [ H.section [P.class_ (ClassName "-note")] $
                renderNote note
            , H.section [P.class_ (ClassName "-options")]
                [ H.ul [P.class_ (ClassName "-style")]
                    [ H.li [] $ renderStyleSelector Normal "Normal"
                    , H.li [] $ renderStyleSelector Dimmed "Dimmed"
                    , H.li [] $ renderStyleSelector Grass "Grass"
                    , H.li [] $ renderStyleSelector Ocean "Ocean"
                    , H.li [] $ renderStyleSelector Peachpuff "Peachpuff"
                    , H.li [] $ renderStyleSelector HotdogStand "Hotdog Stand"
                    ]
                , H.button [E.onClick (E.input_ AddNewVertex)] [H.text "Add New Vertex"]
                ]
            , H.section [P.class_ (ClassName "-children")]
                [ H.ul [] $
                    children
                    # List.mapWithIndex (flip renderChild)
                    # map (H.li [])
                    # List.toUnfoldable
                ]
            ]

    renderNote :: ∀ a. String -> Array (HTML a (Query Unit))
    renderNote note =
        [ H.textarea [ P.class_ (ClassName "nn--autoresize")
                     , E.onValueChange (Just <<< action <<< EditNote)
                     , P.value note
                     ]
        ]

    renderStyleSelector :: ∀ a. Style -> String -> Array (HTML a (Query Unit))
    renderStyleSelector style name =
        [ H.button [ P.class_ (styleClass style)
                   , E.onClick (E.input_ (EditStyle style))
                   ]
            [H.text name]
        ]

    renderChild :: Slot -> VertexID -> Array (ParentHTML Query Query Slot (Monad eff))
    renderChild slot childID =
        let childComponent = defer \_ -> ui childID (Set.insert vertexID parentIDs)
        in [H.slot slot childComponent absurd]

    eval :: Query ~> ParentDSL State Query Query Slot Output (Monad eff)
    eval (Initialize next)
        | isCycle = pure next
        | otherwise = immediate *> subsequent $> next
        where
        immediate = State.put =<< lift (mLiftVertexDSL $ getVertex vertexID)
        subsequent = do
            bus <- lift $ mLiftVertexDSL $ vertexBus
            hoistM mLiftAff $ subscribe $ busEvents bus $ \(Tuple vertexID' vertex) ->
                if vertexID' == vertexID
                    then Just $ true <$ action (UpdateVertex vertex)
                    else Nothing
    eval (UpdateVertex vertex next) = next <$ State.put (Just vertex)
    eval (EditNote note next) = do
        State.get >>= case _ of
            Nothing -> pure unit
            Just (Vertex _ children style) -> do
                let newVertex = (Vertex note children style)
                bus <- lift $ mLiftVertexDSL $ vertexBus
                lift $ mLiftAff $ Bus.write (Tuple vertexID newVertex) bus
        pure next
    eval (EditStyle style next) = do
        State.get >>= case _ of
            Nothing -> pure unit
            Just (Vertex note children _) -> do
                let newVertex = (Vertex note children style)
                bus <- lift $ mLiftVertexDSL $ vertexBus
                lift $ mLiftAff $ Bus.write (Tuple vertexID newVertex) bus
        pure next
    eval (AddNewVertex next) = do
        lift $ mLiftVertexDSL $ newVertex (vertexID : Nil)
        pure next

    initializer :: Maybe (Query Unit)
    initializer = Just $ action Initialize

    finalizer :: Maybe (Query Unit)
    finalizer = Nothing

    isCycle :: Boolean
    isCycle = Set.member vertexID parentIDs

styleClass :: Style -> ClassName
styleClass Normal = ClassName "-style-normal"
styleClass Dimmed = ClassName "-style-dimmed"
styleClass Grass = ClassName "-style-grass"
styleClass Ocean = ClassName "-style-ocean"
styleClass Peachpuff = ClassName "-style-peachpuff"
styleClass HotdogStand = ClassName "-style-hotdog-stand"

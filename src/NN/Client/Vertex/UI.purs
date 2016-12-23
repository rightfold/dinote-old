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
import Control.MonadZero (guard)
import Data.Functor.Coproduct (left, right)
import Data.Lazy (defer)
import Data.Lens ((.~), (<>~))
import Data.List as List
import Data.Set (Set)
import Data.Set as Set
import Halogen.Component.Bus (busEvents)
import Halogen.HTML as H
import Halogen.HTML.Events as E
import Halogen.HTML.Properties as P
import NN.Client.Vertex.DSL (createEdge, createVertex, getVertex, updateVertex, vertexBus, VertexDSL)
import NN.File (FileID)
import NN.Prelude.Halogen
import NN.Vertex (Vertex(..), vertexChildren, VertexID, vertexNote, vertexStyle)
import NN.Vertex.Style (Style(..))

type State = Maybe Vertex

data Query a
    = Initialize a
    | ReplaceVertex Vertex a
    | ModifyVertex (Vertex -> Vertex) a
    | AddNewVertex a

instance functorQuery :: Functor Query where
    map f (Initialize next) = Initialize (f next)
    map f (ReplaceVertex vertex next) = ReplaceVertex vertex (f next)
    map f (ModifyVertex func next) = ModifyVertex func (f next)
    map f (AddNewVertex next) = AddNewVertex (f next)

type Output = Void

type Slot = Int

type Monad eff = Free (Aff (avar :: AVAR | eff) ⊕ VertexDSL)

mLiftAff :: ∀ eff. Aff (avar :: AVAR | eff) ~> Monad eff
mLiftAff = liftF <<< left

mLiftVertexDSL :: ∀ eff. VertexDSL ~> Monad eff
mLiftVertexDSL = liftF <<< right

ui :: ∀ eff. FileID -> VertexID -> Set VertexID -> Component HTML Query Output (Monad eff)
ui fileID vertexID transitiveParentIDs =
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
                     , E.onValueChange (Just <<< action <<< ModifyVertex <<< (vertexNote .~ _))
                     , P.value note
                     ]
        ]

    renderStyleSelector :: ∀ a. Style -> String -> Array (HTML a (Query Unit))
    renderStyleSelector style name =
        [ H.button [ P.class_ (styleClass style)
                   , E.onClick (E.input_ (ModifyVertex $ vertexStyle .~ style))
                   ]
            [H.text name]
        ]

    renderChild :: Slot -> VertexID -> Array (ParentHTML Query Query Slot (Monad eff))
    renderChild slot childID =
        let childComponent = defer \_ -> ui fileID childID (Set.insert vertexID transitiveParentIDs)
        in [H.slot slot childComponent absurd]

    eval :: Query ~> ParentDSL State Query Query Slot Output (Monad eff)
    eval (Initialize next)
        | isCycle = pure next
        | otherwise = immediate *> subsequent $> next
        where
        immediate = State.put =<< lift (mLiftVertexDSL $ getVertex fileID vertexID)
        subsequent = do
            bus <- lift $ mLiftVertexDSL $ vertexBus
            hoistM mLiftAff $ subscribe $ busEvents bus \(Tuple vertexID' vertex) ->
                guard (vertexID' == vertexID)
                $> (Listening <$ action (ReplaceVertex vertex))
    eval (ReplaceVertex vertex next) = next <$ State.put (Just vertex)
    eval (ModifyVertex func next) = do
        State.get >>= case _ of
            Nothing -> pure unit
            Just vertex ->
                let vertex' = func vertex
                in lift $
                    (mLiftVertexDSL vertexBus
                        >>= mLiftAff <<< Bus.write (vertexID /\ vertex'))
                    *> (mLiftVertexDSL $ updateVertex fileID vertexID vertex')
        pure next
    eval (AddNewVertex next) =
        lift (mLiftVertexDSL do
            childID <- createVertex fileID
            createEdge fileID {parentID: vertexID, childID}
            pure childID)
        <#> (vertexChildren <>~ _) <<< List.singleton
        >>= eval <<< ModifyVertex `flip` next

    initializer :: Maybe (Query Unit)
    initializer = Just $ action Initialize

    finalizer :: Maybe (Query Unit)
    finalizer = Nothing

    isCycle :: Boolean
    isCycle = Set.member vertexID transitiveParentIDs

styleClass :: Style -> ClassName
styleClass Normal = ClassName "-style-normal"
styleClass Dimmed = ClassName "-style-dimmed"
styleClass Grass = ClassName "-style-grass"
styleClass Ocean = ClassName "-style-ocean"
styleClass Peachpuff = ClassName "-style-peachpuff"
styleClass HotdogStand = ClassName "-style-hotdog-stand"

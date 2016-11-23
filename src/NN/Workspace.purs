module NN.Workspace
( Query
, Output
, Monad
, ui
) where

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.State.Class as State
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Set (Set)
import Data.Set as Set
import Halogen.HTML as H
import Halogen.HTML.Events as E
import Halogen.HTML.Properties as P
import NN.Auth (UserID(..))
import NN.DSL (NNDSL)
import NN.Prelude.Halogen
import NN.Vertex (VertexID(..))
import NN.Vertex.UI as Vertex.UI

type State =
    { users :: Map UserID (Set VertexID)
    , selectedFileID :: Maybe VertexID
    }

data Query a
    = SelectFile VertexID a

type Output = Void

type Slot = VertexID

type Monad eff = Free (Aff (avar :: AVAR | eff) ⊕ NNDSL)

ui :: ∀ eff. Component HTML Query Output (Monad eff)
ui = parentComponent {initialState, render, eval}
    where
    initialState :: State
    initialState =
        { users: Map.singleton AnonymousID (Set.fromFoldable [VertexID "016c80f4-2931-4943-aefd-96893dcce572", VertexID "9d7c561e-d02b-4cb3-a24e-83d4adb6de55"])
        , selectedFileID: Nothing
        }

    render :: State -> ParentHTML Query Vertex.UI.Query Slot (Monad eff)
    render s =
        H.section [P.class_ (ClassName "nn--workspace")]
            [ H.section [P.class_ (ClassName "-file-tree")] $ renderFileTree s
            , H.section [P.class_ (ClassName "-vertex-tree")] $ renderVertexTree s
            ]

    renderFileTree :: State -> Array (ParentHTML Query Vertex.UI.Query Slot (Monad eff))
    renderFileTree {users, selectedFileID} =
        [H.ul [] $ map (H.li [] <<< renderUserEntry) (List.toUnfoldable $ Map.toList users)]
        where
        renderUserEntry (Tuple userID vertices) =
            [ H.div [P.class_ (ClassName "-user")] [H.text (show userID)]
            , H.ul [P.class_ (ClassName "-files")] $
                map renderFileEntry (Set.toUnfoldable vertices)
            ]
        renderFileEntry vertexID =
            H.li [ E.onClick (E.input_ (SelectFile vertexID))
                 , P.classes $ if Just vertexID == selectedFileID then [ClassName "-active"] else []
                 ]
                [H.text (show vertexID)]

    renderVertexTree :: State -> Array (ParentHTML Query Vertex.UI.Query Slot (Monad eff))
    renderVertexTree {selectedFileID: Nothing} = [H.text "Select a file!"]
    renderVertexTree {selectedFileID: Just vertexID} =
        [H.slot vertexID (defer \_ -> Vertex.UI.ui vertexID Set.empty) absurd]

    eval :: Query ~> ParentDSL State Query Vertex.UI.Query Slot Output (Monad eff)
    eval (SelectFile vertexID next) =
        next <$ State.modify _ {selectedFileID = Just vertexID}

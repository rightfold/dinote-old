module NN.Workspace
( Query
, Output
, Monad
, ui
) where

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
    , selectedFile :: Maybe VertexID
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
        { users: Map.singleton AnonymousID (Set.fromFoldable [VertexID "92eacb4c-a841-4b96-a984-a077caba347c", VertexID "9733d16e-d506-428a-a135-c3e7d886c396"])
        , selectedFile: Nothing
        }

    render :: State -> ParentHTML Query Vertex.UI.Query Slot (Monad eff)
    render s =
        H.section [P.class_ (ClassName "nn--workspace")]
            [ H.section [P.class_ (ClassName "-file-tree")] $ renderFileTree s
            , H.section [P.class_ (ClassName "-vertex-tree")] $ renderVertexTree s
            ]

    renderFileTree :: State -> Array (ParentHTML Query Vertex.UI.Query Slot (Monad eff))
    renderFileTree {users} =
        [H.ul [] $ map (H.li [] <<< renderUserEntry) (List.toUnfoldable $ Map.toList users)]
        where
        renderUserEntry (Tuple userID vertices) =
            [ H.div [P.class_ (ClassName "-user")] [H.text (show userID)]
            , H.ul [P.class_ (ClassName "-files")] $
                map renderFileEntry (Set.toUnfoldable vertices)
            ]
        renderFileEntry vertexID =
            H.li [E.onClick (E.input_ (SelectFile vertexID))]
                [H.text (show vertexID)]

    renderVertexTree :: State -> Array (ParentHTML Query Vertex.UI.Query Slot (Monad eff))
    renderVertexTree {selectedFile: Nothing} = [H.text "Select a file!"]
    renderVertexTree {selectedFile: Just vertexID} =
        [ let rootID = VertexID "92eacb4c-a841-4b96-a984-a077caba347c"
          in H.slot vertexID (defer \_ -> Vertex.UI.ui vertexID Set.empty) absurd
        ]

    eval :: Query ~> ParentDSL State Query Vertex.UI.Query Slot Output (Monad eff)
    eval (SelectFile vertexID next) =
        next <$ State.modify _ {selectedFile = Just vertexID}

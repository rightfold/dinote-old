module NN.Client.Workspace.UI
( Query
, Output
, Monad
, ui
) where

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Free (Free)
import Control.Monad.State.Class as State
import Data.Lazy (defer)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Set as Set
import Halogen.HTML as H
import Halogen.HTML.Events as E
import Halogen.HTML.Properties as P
import NN.Client.Vertex.DSL (VertexDSL)
import NN.Client.Vertex.UI as Vertex.UI
import NN.File (File(..), FileID(..))
import NN.Prelude.Halogen
import NN.Vertex (VertexID(..))

type State =
    { files :: Map FileID File
    , selectedFileID :: Maybe FileID
    }

data Query a
    = SelectFile FileID a

type Output = Void

type Slot = VertexID

type Monad eff = Free (Aff (avar :: AVAR | eff) ⊕ VertexDSL)

ui :: ∀ eff. Component HTML Query Output (Monad eff)
ui = parentComponent {initialState, render, eval}
    where
    initialState :: State
    initialState =
        { files:
            Map.empty
            # Map.insert (FileID "ab77b629-06b1-4c2e-a1e2-11ec36d778e8") (File "A" (VertexID "d7b77961-4b36-4d76-b1b0-db4851c9fdae"))
            # Map.insert (FileID "86fb5f54-0fc3-4aee-b7ba-df4844d12f18") (File "B" (VertexID "3a4c0d0f-28e3-42d3-a2de-430bdf569839"))
        , selectedFileID: Nothing
        }

    render :: State -> ParentHTML Query Vertex.UI.Query Slot (Monad eff)
    render s =
        H.section [P.class_ (ClassName "nn--workspace")]
            [ H.section [P.class_ (ClassName "-file-tree")] $ renderFileTree s
            , H.section [P.class_ (ClassName "-vertex-tree")] $ renderVertexTree s
            ]

    renderFileTree :: State -> Array (ParentHTML Query Vertex.UI.Query Slot (Monad eff))
    renderFileTree {files, selectedFileID} =
        [H.ul [] $ map renderFileEntry (List.toUnfoldable $ Map.toList files)]
        where
        renderFileEntry (fileID /\ File name _) =
            H.li [ E.onClick (E.input_ (SelectFile fileID))
                 , P.classes $ if Just fileID == selectedFileID then [ClassName "-active"] else []
                 ]
                [H.text name]

    renderVertexTree :: State -> Array (ParentHTML Query Vertex.UI.Query Slot (Monad eff))
    renderVertexTree {files, selectedFileID} =
        case selectedFileID >>= \fileID -> (fileID /\ _) <$> Map.lookup fileID files of
            Nothing -> [H.text "Select a file!"]
            Just (fileID /\ File _ vertexID) ->
                [H.slot vertexID (defer \_ -> Vertex.UI.ui fileID vertexID Set.empty) absurd]

    eval :: Query ~> ParentDSL State Query Vertex.UI.Query Slot Output (Monad eff)
    eval (SelectFile fileID next) =
        next <$ State.modify _ {selectedFileID = Just fileID}

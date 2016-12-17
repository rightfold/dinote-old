module NN.Client.UI
( Query
, Output(..)
, Monad
, ui
) where

import Control.Monad.Free (Free, liftF)
import Control.Monad.State as State
import Data.Const (Const)
import Data.Functor.Coproduct (left, right)
import Data.Lazy (defer)
import Halogen.Component (hoist)
import Halogen.Component.ChildPath as CP
import Halogen.HTML as H
import NN.Client.Authentication.UI as Authentication.UI
import NN.Client.Workspace.UI as Workspace.UI
import NN.Prelude.Halogen
import NN.User (UserID)

data State
    = Authenticated
    | NotAuthenticated

data Query a
    = SignedIn UserID a

type ChildQuery = Workspace.UI.Query ⊕ Authentication.UI.Query ⊕ Const Void

type Output = Void

type Slot = Unit + Unit + Void

type Monad eff = Free (Workspace.UI.Monad eff ⊕ Authentication.UI.Monad)

ui :: ∀ eff. Component HTML Query Output (Monad eff)
ui = parentComponent {initialState, render, eval}
    where
    initialState :: State
    initialState = NotAuthenticated

    render :: State -> ParentHTML Query ChildQuery Slot (Monad eff)
    render Authenticated    = H.slot' CP.cp1 unit (defer \_ -> hoist (liftF <<< left)  Workspace.UI.ui)      absurd
    render NotAuthenticated = H.slot' CP.cp2 unit (defer \_ -> hoist (liftF <<< right) Authentication.UI.ui) handle
        where handle (Authentication.UI.SignedIn userID) = Just $ action (SignedIn userID)

    eval :: Query ~> ParentDSL State Query ChildQuery Slot Output (Monad eff)
    eval (SignedIn _ next) = next <$ State.put Authenticated

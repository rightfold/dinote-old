module NN.Client.UI
( Query
, Input
, Output(..)
, Monad
, ui
) where

import Control.Monad.Free (Free, liftF)
import Control.Monad.State as State
import Data.Const (Const)
import Data.Functor.Coproduct (left, right)
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

type Input = Unit

type Output = Void

type Slot = Unit + Unit + Void

type Monad eff = Free (Workspace.UI.Monad eff ⊕ Authentication.UI.Monad)

ui :: ∀ eff. Component HTML Query Input Output (Monad eff)
ui = parentComponent {initialState, render, eval, receiver: const Nothing}
    where
    initialState :: Input -> State
    initialState _ = NotAuthenticated

    render :: State -> ParentHTML Query ChildQuery Slot (Monad eff)
    render Authenticated    = H.slot' CP.cp1 unit (hoist (liftF <<< left)  Workspace.UI.ui)      unit absurd
    render NotAuthenticated = H.slot' CP.cp2 unit (hoist (liftF <<< right) Authentication.UI.ui) unit handle
        where handle (Authentication.UI.SignedIn userID) = Just $ action (SignedIn userID)

    eval :: Query ~> ParentDSL State Query ChildQuery Slot Output (Monad eff)
    eval (SignedIn _ next) = next <$ State.put Authenticated

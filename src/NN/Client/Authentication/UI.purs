module NN.Client.Authentication.UI
( Query
, Input
, Output(..)
, Monad
, ui
) where

import Control.Monad.State.Class as State
import Control.Monad.Trans.Class (lift)
import Data.Password (Password(..))
import Halogen.HTML as H
import Halogen.HTML.Events as E
import Halogen.HTML.Properties as P
import NN.Client.Authentication.DSL (authenticate, AuthenticationDSL)
import NN.Prelude.Halogen
import NN.User (UserID)

type State =
    { username :: String
    , password :: Password
    }

data Query a
    = Alter (State -> State) a
    | Submit a

type Input = Unit

newtype Output = SignedIn UserID

type Monad = AuthenticationDSL

ui :: Component HTML Query Input Output Monad
ui = component {initialState, render, eval, receiver: const Nothing}
    where
    initialState :: Input -> State
    initialState _ = {username: "", password: Password ""}

    render :: State -> ComponentHTML Query
    render {username, password: Password password} =
        H.div []
            [ H.input [ P.type_ P.InputEmail
                      , E.onValueChange (E.input \s -> Alter _ { username = s })
                      ]
            , H.input [ P.type_ P.InputPassword
                      , E.onValueChange (E.input \s -> Alter _ { password = Password s })
                      ]
            , H.button [ P.type_ P.ButtonSubmit
                       , E.onClick (E.input_ Submit)
                       ]
                [H.text "Sign In"]
            ]

    eval :: Query ~> ComponentDSL State Query Output Monad
    eval (Alter f next) = next <$ State.modify f
    eval (Submit next) = do
        {username, password} <- State.get
        lift (authenticate username password) >>= case _ of
            Just userID -> raise $ SignedIn userID
            Nothing -> pure unit
        pure next

module NN.Client.Authentication.UI
( Query
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

newtype Output = SignedIn UserID

type Monad = AuthenticationDSL

ui :: Component HTML Query Output Monad
ui = component {initialState, render, eval}
    where
    initialState :: State
    initialState = {username: "", password: Password ""}

    render :: State -> ComponentHTML Query
    render {username, password: Password password} =
        H.div []
            [ H.input [ P.inputType P.InputEmail
                      , E.onValueChange (E.input \s -> Alter _ { username = s })
                      ]
            , H.input [ P.inputType P.InputPassword
                      , E.onValueChange (E.input \s -> Alter _ { password = Password s })
                      ]
            , H.button [ P.buttonType P.ButtonSubmit
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

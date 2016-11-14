module NN.Workspace
( Query
, Output
, ui
) where

import Halogen.Component (Component, component, ComponentDSL)
import Halogen.HTML (HTML)
import Halogen.HTML as H
import NN.Monad (NN)
import NN.Prelude

type State = Unit

newtype Query a = Query Void

type Output = Void

ui :: Component HTML Query Output NN
ui = component {initialState, render, eval}
    where
    initialState :: State
    initialState = unit

    render :: State -> HTML Void (Query Unit)
    render _ = H.strong [] [H.text "Hello, world!"]

    eval :: Query ~> ComponentDSL State Query Output NN
    eval (Query void) = absurd void

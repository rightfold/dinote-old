module NN.Workspace
( Query
, Output
, ui
) where

import Halogen.Component (Component, component, ComponentDSL)
import Halogen.HTML (HTML)
import Halogen.HTML as H
import NN.DSL (NNDSL)
import NN.Prelude

type State = Unit

newtype Query a = Query Void

type Output = Void

ui :: Component HTML Query Output NNDSL
ui = component {initialState, render, eval}
    where
    initialState :: State
    initialState = unit

    render :: State -> HTML Void (Query Unit)
    render _ = H.strong [] [H.text "Hello, world!"]

    eval :: Query ~> ComponentDSL State Query Output NNDSL
    eval (Query void) = absurd void

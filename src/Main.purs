module Main
( main
, main'
) where

import Halogen.Component as Halogen.Component
import Halogen.Util (awaitBody, runHalogenAff)
import Halogen.VirtualDOM.Driver (runUI)
import NN.Effects (NNEffects)
import NN.Interpret as NN.Interpret
import NN.Prelude
import NN.Workspace as Workspace

main :: ∀ eff. Eff (NNEffects eff) Unit
main = runHalogenAff main'

main' :: ∀ eff. Aff (NNEffects eff) Unit
main' = awaitBody >>= runUI (Halogen.Component.interpret NN.Interpret.interpret Workspace.ui) # void

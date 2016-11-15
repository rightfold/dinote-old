module Main
( main
, main'
) where

import Halogen.Component (interpret)
import Halogen.Util (awaitBody, runHalogenAff)
import Halogen.VirtualDOM.Driver (runUI)
import NN.DSL (runNNDSL)
import NN.Effects (NNEffects)
import NN.Prelude
import NN.Workspace as Workspace

main :: forall eff. Eff (NNEffects eff) Unit
main = runHalogenAff main'

main' :: forall eff. Aff (NNEffects eff) Unit
main' = awaitBody >>= runUI (interpret runNNDSL Workspace.ui) # void

module Main.Client
( main
, main'
) where

import Control.Monad.Aff.Bus as Bus
import Network.HTTP.Affjax (AJAX)
import Halogen.Component as Halogen.Component
import Halogen.Effects (HalogenEffects)
import Halogen.Util (awaitBody, runHalogenAff)
import Halogen.VirtualDOM.Driver (runUI)
import NN.Client.DSL.Interpret (interpret)
import NN.Client.Workspace.UI as Workspace.UI
import NN.Prelude

main :: ∀ eff. Eff (HalogenEffects (ajax :: AJAX | eff)) Unit
main = runHalogenAff main'

main' :: ∀ eff. Aff (HalogenEffects (ajax :: AJAX | eff)) Unit
main' = do
    vertexBus <- Bus.make
    awaitBody >>= runUI (Halogen.Component.interpret (interpret vertexBus) Workspace.UI.ui) # void

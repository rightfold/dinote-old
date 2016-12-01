module Main.Client
( main
, main'
) where

import Control.Monad.Aff (forkAff)
import Control.Monad.Aff.Bus as Bus
import Control.Monad.Rec.Class (forever)
import Network.HTTP.Affjax (AJAX)
import Halogen.Aff.Util (awaitBody, runHalogenAff)
import Halogen.Component as Halogen.Component
import Halogen.Effects (HalogenEffects)
import Halogen.VirtualDOM.Driver (runUI)
import NN.Client.DSL.Interpret (interpret)
import NN.Client.Workspace.UI as Workspace.UI
import NN.Prelude

main :: ∀ eff. Eff (HalogenEffects (ajax :: AJAX | eff)) Unit
main = runHalogenAff main'

main' :: ∀ eff. Aff (HalogenEffects (ajax :: AJAX | eff)) Unit
main' = do
    vertexBus <- Bus.make
    forkAff $ forever do
        Tuple vertexID vertex <- Bus.read vertexBus
        traceAnyA vertexID
        traceAnyA vertex
    awaitBody >>= runUI (Halogen.Component.hoist (interpret vertexBus) Workspace.UI.ui) # void

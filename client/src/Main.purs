module Main
( main
, main'
) where

import Control.Monad.Eff.Ref (newRef)
import Data.Map as Map
import Network.HTTP.Affjax (AJAX)
import Halogen.Component as Halogen.Component
import Halogen.Effects (HalogenEffects)
import Halogen.Util (awaitBody, runHalogenAff)
import Halogen.VirtualDOM.Driver (runUI)
import NN.Interpret as NN.Interpret
import NN.Prelude
import NN.Workspace as Workspace

main :: ∀ eff. Eff (HalogenEffects (ajax :: AJAX | eff)) Unit
main = runHalogenAff main'

main' :: ∀ eff. Aff (HalogenEffects (ajax :: AJAX | eff)) Unit
main' = do
    busesRef <- liftEff $ newRef Map.empty
    awaitBody >>= runUI (Halogen.Component.interpret (NN.Interpret.interpret busesRef) Workspace.ui) # void
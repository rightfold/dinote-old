module Main
( main
, main'
) where

import Control.Monad.Eff.Ref (newRef)
import Data.Map as Map
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
main' = do
    busesRef <- liftEff $ newRef Map.empty
    awaitBody >>= runUI (Halogen.Component.interpret (NN.Interpret.interpret busesRef) Workspace.ui) # void

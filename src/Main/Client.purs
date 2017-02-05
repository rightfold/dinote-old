module Main.Client
( main
, main'
) where

import Control.Monad.Aff (forkAff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Bus (BusRW)
import Control.Monad.Aff.Bus as Bus
import Control.Monad.Aff.Skull as Skull
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Free (foldFree)
import Control.Monad.Rec.Class (forever)
import Data.Functor.Coproduct (coproduct)
import Halogen.Aff (HalogenEffects)
import Halogen.Aff.Util (awaitBody, runHalogenAff)
import Halogen.Component as Halogen.Component
import Halogen.VDom.Driver (runUI)
import Network.HTTP.Affjax (AJAX)
import NN.Client.Authentication.DSL.Interpret.HTTP (runAuthenticationDSL)
import NN.Client.UI as Client.UI
import NN.Client.Vertex.DSL.Interpret.HTTP (runVertexDSLF)
import NN.Client.Vertex.HTTP (getVertexBatcher)
import NN.File (FileID)
import NN.Prelude
import NN.Vertex (Vertex, VertexID)

main :: ∀ eff. Eff (HalogenEffects (ajax :: AJAX | eff)) Unit
main = runHalogenAff main'

main' :: ∀ eff. Aff (HalogenEffects (ajax :: AJAX | eff)) Unit
main' = do
    vertexBus <- Bus.make
    forkAff $ forever do
        Tuple vertexID vertex <- Bus.read vertexBus
        traceAnyA vertexID
        traceAnyA vertex
    getVertexSkull <- liftEff $ Skull.newState getVertexBatcher
    awaitBody >>= runUI (Halogen.Component.hoist (interpret vertexBus getVertexSkull) Client.UI.ui) unit >>> void

interpret
    :: ∀ eff
     . BusRW (Tuple VertexID Vertex)
    -> Skull.State (ajax :: AJAX, avar :: AVAR, ref :: REF | eff) (FileID × VertexID) (Maybe Vertex)
    -> Client.UI.Monad (ajax :: AJAX, ref :: REF | eff)
    ~> Aff (ajax :: AJAX, avar :: AVAR, ref :: REF | eff)
interpret vertexBus getVertexSkull = foldFree (coproduct (foldFree (coproduct id (foldFree (runVertexDSLF vertexBus getVertexSkull)))) runAuthenticationDSL)

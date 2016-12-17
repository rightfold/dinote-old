module Main.Client
( main
, main'
) where

import Control.Monad.Aff (forkAff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Bus (BusRW)
import Control.Monad.Aff.Bus as Bus
import Control.Monad.Free (foldFree)
import Control.Monad.Rec.Class (forever)
import Data.Functor.Coproduct (coproduct)
import Network.HTTP.Affjax (AJAX)
import Halogen.Aff.Util (awaitBody, runHalogenAff)
import Halogen.Component as Halogen.Component
import Halogen.Effects (HalogenEffects)
import Halogen.VirtualDOM.Driver (runUI)
import NN.Client.Authentication.DSL.Interpret.HTTP (runAuthenticationDSL)
import NN.Client.Authentication.UI as Authentication.UI
import NN.Client.UI as Client.UI
import NN.Client.Vertex.DSL (VertexDSL)
import NN.Client.Vertex.DSL.Interpret.HTTP (runVertexDSLF)
import NN.Client.Workspace.UI as Workspace.UI
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
    awaitBody >>= runUI (Halogen.Component.hoist (interpret vertexBus) Client.UI.ui) # void

interpret
    :: ∀ eff
     . BusRW (Tuple VertexID Vertex)
    -> Client.UI.Monad (ajax :: AJAX | eff)
    ~> Aff (ajax :: AJAX, avar :: AVAR | eff)
interpret vertexBus = foldFree go1
    where
    go1 :: Workspace.UI.Monad (ajax :: AJAX | eff) ⊕ Authentication.UI.Monad ~> Aff (ajax :: AJAX, avar :: AVAR | eff)
    go1 = coproduct onLeft1 runAuthenticationDSL

    onLeft1 :: Workspace.UI.Monad (ajax :: AJAX | eff) ~> Aff (ajax :: AJAX, avar :: AVAR | eff)
    onLeft1 = foldFree go2

    go2 :: Aff (ajax :: AJAX, avar :: AVAR | eff) ⊕ VertexDSL ~> Aff (ajax :: AJAX, avar :: AVAR | eff)
    go2 = coproduct id (foldFree (runVertexDSLF vertexBus))

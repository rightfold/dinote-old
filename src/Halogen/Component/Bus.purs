module Halogen.Component.Bus
( busEvents
) where

import Control.Monad.Aff (runAff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Bus (Bus)
import Control.Monad.Aff.Bus as Bus
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Rec.Class (forever)
import Halogen.Query.EventSource (EventSource, eventSource)
import NN.Prelude

busEvents
    :: ∀ r eff m a f
     . (MonadAff (avar :: AVAR | eff) m)
    => Bus (read :: Bus.Cap | r) a
    -> (a -> Maybe (f Boolean))
    -> EventSource f m
busEvents bus handler = eventSource attach handle
    where
    attach k = void $
        runAff (\_ -> pure unit) (\_ -> pure unit) $
            forever $ Bus.read bus >>= liftEff <<< k
    handle = handler

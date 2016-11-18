module Halogen.Component.Bus
( busEvents
) where

import Control.Monad.Aff.Bus (Bus)
import Control.Monad.Aff.Bus as Bus
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

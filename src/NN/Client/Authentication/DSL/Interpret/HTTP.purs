module NN.Client.Authentication.DSL.Interpret.HTTP
( runAuthenticationDSL
, runAuthenticationDSLF
) where

import Control.Monad.Free (foldFree)
import NN.Client.Authentication.DSL (AuthenticationDSL, AuthenticationDSLF(..))
import NN.Prelude
import NN.User (UserID(..))

runAuthenticationDSL
    :: ∀ eff
     . AuthenticationDSL
    ~> Aff eff
runAuthenticationDSL = foldFree runAuthenticationDSLF

runAuthenticationDSLF
    :: ∀ eff
     . AuthenticationDSLF
    ~> Aff eff
runAuthenticationDSLF (Authenticate username password next) =
    pure $ next (Just (UserID "test"))

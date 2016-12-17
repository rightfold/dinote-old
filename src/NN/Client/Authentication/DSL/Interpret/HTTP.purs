module NN.Client.Authentication.DSL.Interpret.HTTP
( runAuthenticationDSL
, runAuthenticationDSLF
) where

import Control.Monad.Free (foldFree)
import Data.Sexp as Sexp
import Data.Sexp (fromSexp, toSexp)
import Network.HTTP.Affjax (AJAX)
import Network.HTTP.Affjax as Affjax
import Network.HTTP.StatusCode (StatusCode(..))
import NN.Client.Authentication.DSL (AuthenticationDSL, AuthenticationDSLF(..))
import NN.Prelude

runAuthenticationDSL
    :: ∀ eff
     . AuthenticationDSL
    ~> Aff (ajax :: AJAX | eff)
runAuthenticationDSL = foldFree runAuthenticationDSLF

runAuthenticationDSLF
    :: ∀ eff
     . AuthenticationDSLF
    ~> Aff (ajax :: AJAX | eff)
runAuthenticationDSLF (Authenticate username password next) =
    Affjax.post "/api/v1/session" (Sexp.toString $ toSexp (username /\ unwrap password))
    <#> next <<< case _ of
        {status: StatusCode 200, response} -> fromSexp =<< Sexp.fromString response
        {status: _} -> Nothing

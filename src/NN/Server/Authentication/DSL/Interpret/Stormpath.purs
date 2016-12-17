module NN.Server.Authentication.DSL.Interpret.Stormpath
( runAuthenticationDSL
, runAuthenticationDSLF
) where

import Control.Monad.Error.Class (catchError)
import Control.Monad.Free (foldFree)
import Database.Stormpath (STORMPATH)
import Database.Stormpath as Stormpath
import NN.Prelude
import NN.Server.Authentication.DSL (AuthenticationDSL, AuthenticationDSLF(..))
import NN.User (UserID(..))

runAuthenticationDSL
    :: ∀ eff
     . Stormpath.Application
    -> AuthenticationDSL
    ~> Aff (stormpath :: STORMPATH | eff)
runAuthenticationDSL = foldFree <<< runAuthenticationDSLF

runAuthenticationDSLF
    :: ∀ eff
     . Stormpath.Application
    -> AuthenticationDSLF
    ~> Aff (stormpath :: STORMPATH | eff)
runAuthenticationDSLF stormpath (Authenticate username password next) =
    catchError (Just <$> Stormpath.authenticateAccount stormpath username password)
               (const $ pure Nothing)
    <#> map (Stormpath.accountHref >>> UserID) >>> next

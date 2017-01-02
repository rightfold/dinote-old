module NN.Server.Authentication.DSL.Interpret.DB
( runAuthenticationDSL
, runAuthenticationDSLF
) where

import Control.Monad.Error.Class (catchError)
import Control.Monad.Free (foldFree)
import Database.PostgreSQL (POSTGRESQL)
import Database.PostgreSQL as PostgreSQL
import Database.Stormpath (STORMPATH)
import Database.Stormpath as Stormpath
import NN.Prelude
import NN.Server.Authentication.DSL (AuthenticationDSL, AuthenticationDSLF(..))
import NN.Server.Session.DB (createSession)
import NN.Session (Session(..))
import NN.User (UserID(..))

runAuthenticationDSL
    :: ∀ eff
     . PostgreSQL.Connection
    -> Stormpath.Application
    -> AuthenticationDSL
    ~> Aff (postgreSQL :: POSTGRESQL, stormpath :: STORMPATH | eff)
runAuthenticationDSL conn stormpath =
    foldFree (runAuthenticationDSLF conn stormpath :: _ ~> _)

runAuthenticationDSLF
    :: ∀ eff
     . PostgreSQL.Connection
    -> Stormpath.Application
    -> AuthenticationDSLF
    ~> Aff (postgreSQL :: POSTGRESQL, stormpath :: STORMPATH | eff)
runAuthenticationDSLF conn stormpath (Authenticate username password next) =
    catchError (Just <$> Stormpath.authenticateAccount stormpath username password)
                (const $ pure Nothing)
    <#> map (Stormpath.accountHref >>> UserID)
    >>= case _ of
        Just userID -> do
            sessionID <- createSession conn (Session userID "")
            pure $ next $ Just (sessionID /\ userID)
        Nothing -> pure $ next Nothing

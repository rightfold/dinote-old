module NN.Server.Session.DB
( createSession
) where

import Database.PostgreSQL (Connection, POSTGRESQL, query)
import NN.Prelude
import NN.Session (SessionID(..))
import NN.User (UserID(..))
import Partial.Unsafe (unsafeCrashWith)

createSession
    :: ∀ eff
     . Connection
    -> UserID
    -> String
    -> Aff (postgreSQL :: POSTGRESQL | eff) SessionID
createSession conn (UserID userID) description =
    query conn """
        INSERT INTO sessions (id, user_id, description)
        VALUES (gen_random_uuid(), $1, $2)
        RETURNING id
    """ (userID /\ description /\ unit)
    <#> case _ of
        [sessionID /\ (_ :: Unit)] -> SessionID sessionID
        _ -> unsafeCrashWith "createSession"

module NN.Server.User.DB
( authenticate
) where

import Database.PostgreSQL (Connection, POSTGRESQL, query)
import NN.Prelude
import NN.User (UserID(..))

authenticate
    :: ∀ eff
     . Connection
    -> String
    -> String
    -> Aff (postgreSQL :: POSTGRESQL | eff) (Maybe UserID)
authenticate conn emailAddress password =
    query conn """
        SELECT id
        FROM users
        WHERE
                lower(email_address) = lower($1)
            AND crypt($2, password_hash) = password_hash
    """ (emailAddress /\ password /\ unit)
    <#> case _ of
        [userID /\ (_ :: Unit)] -> Just (UserID userID)
        _ -> Nothing

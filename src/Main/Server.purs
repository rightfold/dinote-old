module Main.Server
( main
) where

import Data.Map as Map
import Database.PostgreSQL (newPool, Pool, POSTGRESQL, withConnection)
import Network.HTTP.Message (Request, Response)
import Network.HTTP.Node (nodeHandler)
import Node.HTTP (createServer, listen)
import NN.Prelude

main = launchAff do
    db <- newPool { user: "postgres"
                  , password: "lol123"
                  , host: "localhost"
                  , port: 5432
                  , database: "nn"
                  , max: 10
                  , idleTimeoutMillis: 0
                  }

    liftEff do
        server <- createServer $ nodeHandler $ handler db
        listen server {hostname: "localhost", port: 1337, backlog: Nothing} (pure unit)

handler
    :: ∀ eff
     . Pool
    -> Request (postgreSQL :: POSTGRESQL | eff)
    -> Aff (postgreSQL :: POSTGRESQL | eff) (Response (postgreSQL :: POSTGRESQL | eff))
handler db req =
    withConnection db \conn -> do
        pure { status: {code: 200, message: "OK"}
            , headers: Map.singleton (CaseInsensitiveString "Content-Type") "text/html"
            , body: pure unit
            }

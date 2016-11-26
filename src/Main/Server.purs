module Main.Server
( main
) where

import Control.Coroutine (emit)
import Data.Map as Map
import Data.Sexp as Sexp
import Database.PostgreSQL (newPool, Pool, POSTGRESQL, withConnection)
import Network.HTTP.Message (Request, Response)
import Network.HTTP.Node (nodeHandler)
import Node.Buffer as Buffer
import Node.Encoding (Encoding(UTF8))
import Node.HTTP (createServer, listen)
import NN.Prelude
import NN.Vertex (Vertex(..))
import NN.Vertex.Style (Style(..))

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
             , headers:
                Map.empty
                # Map.insert (CaseInsensitiveString "Content-Type") "application/x-sexp"
                # Map.insert (CaseInsensitiveString "Access-Control-Allow-Origin") "*"
             , body:
                let v = Vertex "Hello, world!" Nil Ocean
                in emit $ unsafePerformEff $ Buffer.fromString (Sexp.toString $ Sexp.toSexp v) UTF8
             }

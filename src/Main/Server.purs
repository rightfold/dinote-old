module Main.Server
( main
) where

import Control.Coroutine (emit)
import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Sexp as Sexp
import Data.String as String
import Database.PostgreSQL (Connection, execute, newPool, Pool, POSTGRESQL, query, withConnection)
import Network.HTTP.Message (Request, Response)
import Network.HTTP.Node (nodeHandler)
import Node.Buffer as Buffer
import Node.Encoding (Encoding(UTF8))
import Node.HTTP (createServer, listen)
import NN.Prelude
import NN.Vertex (Vertex(..), VertexID(..))
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

    withConnection db setupDB

    liftEff do
        server <- createServer $ nodeHandler $ handle db
        listen server {hostname: "localhost", port: 1337, backlog: Nothing} (pure unit)

handle
    :: ∀ eff
     . Pool
    -> Request (postgreSQL :: POSTGRESQL | eff)
    -> Aff (postgreSQL :: POSTGRESQL | eff) (Response (postgreSQL :: POSTGRESQL | eff))
handle db req =
    case String.split (String.Pattern "/") req.path of
        ["", "api", "v1", "vertices", vertexID] -> handleVertex db (VertexID vertexID)
        _ -> pure notFound

handleVertex
    :: ∀ eff
     . Pool
    -> VertexID
    -> Aff (postgreSQL :: POSTGRESQL | eff) (Response (postgreSQL :: POSTGRESQL | eff))
handleVertex db (VertexID vertexID) =
    withConnection db \conn -> do
        result <- query conn """
            SELECT
                v.note,
                CASE WHEN count(e.*) = 0 THEN
                    ARRAY[] :: uuid[]
                ELSE
                    array_agg(e.child_id ORDER BY e.index ASC)
                END,
                v.style
            FROM vertices AS v
            LEFT JOIN edges AS e
                ON e.parent_id = v.id
            WHERE v.id = $1
            GROUP BY v.id
        """ (Tuple vertexID unit)
        case result of
            [note /\ children /\ style /\ (_ :: Unit)] ->
                pure { status: {code: 200, message: "OK"}
                     , headers:
                        Map.empty
                        # Map.insert (CaseInsensitiveString "Content-Type") "application/x-sexp"
                        # Map.insert (CaseInsensitiveString "Access-Control-Allow-Origin") "*"
                     , body:
                        let s = case style of
                                    "normal              " -> Normal
                                    "dimmed              " -> Dimmed
                                    "grass               " -> Grass
                                    "ocean               " -> Ocean
                                    "peachpuff           " -> Peachpuff
                                    "hotdog_stand        " -> HotdogStand
                                    _ -> Normal
                            v = Vertex note (Array.toUnfoldable $ map VertexID children) s
                        in emit $ unsafePerformEff $ Buffer.fromString (Sexp.toString $ Sexp.toSexp v) UTF8
                     }
            _ -> pure notFound

notFound :: ∀ eff. Response eff
notFound =
    { status: {code: 404, message: "Not Found"}
    , headers: Map.empty :: Map CaseInsensitiveString String
    , body: pure unit
    }

setupDB :: ∀ eff. Connection -> Aff (postgreSQL :: POSTGRESQL | eff) Unit
setupDB conn = do
    execute conn """
        CREATE TABLE IF NOT EXISTS users (
            id              uuid        NOT NULL,
            name            text        NOT NULL,
            email_address   char(254)   NOT NULL,
            password_hash   bytea       NOT NULL,
            PRIMARY KEY (id)
        )
    """ unit

    execute conn """
        CREATE UNIQUE INDEX IF NOT EXISTS users__email_address
            ON users
            (lower(email_address))
    """ unit

    execute conn """
        CREATE TABLE IF NOT EXISTS vertices (
            id          uuid        NOT NULL,
            note        text        NOT NULL,
            style       char(20)    NOT NULL,
            PRIMARY KEY (id)
        )
    """ unit

    execute conn """
        CREATE TABLE IF NOT EXISTS edges (
            parent_id   uuid        NOT NULL,
            child_id    uuid        NOT NULL,
            index       int         NOT NULL,
            PRIMARY KEY (parent_id, child_id, index),
            FOREIGN KEY (parent_id)
                REFERENCES vertices(id)
                ON DELETE CASCADE,
            FOREIGN KEY (child_id)
                REFERENCES vertices(id)
                ON DELETE CASCADE
        )
    """ unit

    execute conn """
        CREATE INDEX IF NOT EXISTS edges__child_id
            ON edges
            (child_id)
    """ unit

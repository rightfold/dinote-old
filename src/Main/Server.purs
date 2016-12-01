module Main.Server
( main
) where

import Control.Coroutine (emit)
import Control.Monad.Aff (launchAff)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Sexp as Sexp
import Data.String as String
import Data.String.CaseInsensitive (CaseInsensitiveString(..))
import Data.UUID (GENUUID)
import Data.UUID as UUID
import Database.PostgreSQL (Connection, execute, newPool, Pool, POSTGRESQL, query, withConnection)
import Network.HTTP.Message (Request, Response)
import Network.HTTP.Node (nodeHandler)
import Node.Buffer as Buffer
import Node.Encoding (Encoding(UTF8))
import Node.FS (FS)
import Node.FS.Sync (readFile)
import Node.HTTP (createServer, listen)
import NN.Prelude
import NN.Server.Setup (setupDB)
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
    -> Request (fs :: FS, uuid :: GENUUID, postgreSQL :: POSTGRESQL | eff)
    -> Aff (fs :: FS, uuid :: GENUUID, postgreSQL :: POSTGRESQL | eff) (Response (fs :: FS, uuid :: GENUUID, postgreSQL :: POSTGRESQL | eff))
handle db req =
    case String.split (String.Pattern "/") req.path of
        ["", ""] -> static "text/html" "index.html"
        ["", "output", "nn.js"] -> static "application/javascript" "output/nn.js"
        ["", "output", "nn.css"] -> static "text/css" "output/nn.css"
        ["", "api", "v1", "vertices"] -> handleCreateVertex db
        ["", "api", "v1", "vertices", vertexID] -> handleVertex db (VertexID vertexID)
        ["", "api", "v1", "vertices", parentID, "children", childID] ->
            handleCreateEdge db {parentID: VertexID parentID, childID: VertexID childID}
        _ -> pure notFound

static :: ∀ eff. String -> String -> Aff (fs :: FS | eff) (Response (fs :: FS | eff))
static mime path = do
    contents <- liftEff' $ readFile path
    pure { status: {code: 200, message: "OK"}
         , headers: Map.singleton (CaseInsensitiveString "content-type") mime
         , body: emit contents
         }

handleCreateVertex
    :: ∀ eff
     . Pool
    -> Aff (uuid :: GENUUID, postgreSQL :: POSTGRESQL | eff) (Response (uuid :: GENUUID, postgreSQL :: POSTGRESQL | eff))
handleCreateVertex db =
    withConnection db \conn -> do
        vertexIDStr <- liftEff $ show <$> UUID.genUUID
        execute conn """
            INSERT INTO vertices (id, note, style)
            VALUES ($1, '', 'normal')
        """ (vertexIDStr /\ unit)
        let vertexID = VertexID vertexIDStr
        pure { status: {code: 200, message: "OK"}
             , headers: Map.empty :: Map CaseInsensitiveString String
             , body: emit $ unsafePerformEff $ Buffer.fromString (Sexp.toString $ Sexp.toSexp vertexID) UTF8
             }

handleCreateEdge
    :: ∀ eff
     . Pool
    -> {parentID :: VertexID, childID :: VertexID}
    -> Aff (postgreSQL :: POSTGRESQL | eff) (Response (postgreSQL :: POSTGRESQL | eff))
handleCreateEdge db {parentID: VertexID parentID, childID: VertexID childID} =
    withConnection db \conn -> do
        execute conn """
            INSERT INTO edges (parent_id, child_id, index)
            SELECT $1, $2, coalesce(max(index) + 1, 0)
            FROM edges
            WHERE parent_id = $1
        """ (parentID /\ childID /\ unit)
        pure { status: {code: 200, message: "OK"}
             , headers: Map.empty :: Map CaseInsensitiveString String
             , body: pure unit
             }

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
        """ (vertexID /\ unit)
        case result of
            [note /\ children /\ style /\ (_ :: Unit)] ->
                pure { status: {code: 200, message: "OK"}
                     , headers: Map.empty :: Map CaseInsensitiveString String
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

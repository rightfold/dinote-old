module Main.Server
( main
) where

import Control.Monad.Aff (launchAff)
import Control.Monad.Free (foldFree)
import Control.Monad.Maybe.Trans (MaybeT, runMaybeT)
import Control.Monad.Reader.Trans (runReaderT)
import Control.Monad.Trans.Class (lift)
import Data.ByteString as ByteString
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Sexp as Sexp
import Data.String as String
import Data.String.CaseInsensitive (CaseInsensitiveString(..))
import Data.UUID (GENUUID)
import Database.PostgreSQL (Connection, newPool, Pool, POSTGRESQL, withConnection)
import Database.Stormpath (STORMPATH)
import Database.Stormpath as Stormpath
import Network.HTTP.Cookie (getCookie, setCookie)
import Network.HTTP.Message (Request, Response)
import Network.HTTP.Node (nodeHandler)
import Node.Encoding (Encoding(UTF8))
import Node.FS (FS)
import Node.FS.Sync (readFile)
import Node.HTTP (createServer, listen)
import Node.Process (exit, lookupEnv)
import NN.File (FileID(..))
import NN.Prelude
import NN.Server.Authorization.DSL.Interpret.DB (runAuthorizationDSL)
import NN.Server.Setup (setupDB)
import NN.Server.Vertex.DSL (VertexDSL, VertexDSLF)
import NN.Server.Vertex.DSL.Interpret.Authorization as Vertex.DSL.Interpret.Authorization
import NN.Server.Vertex.DSL.Interpret.DB as Vertex.DSL.Interpret.DB
import NN.Server.Vertex.Web (handleVertexAPI)
import NN.User (UserID(..))

main = launchAff do
    stormpath <- do
        apiKeyIDM     <- liftEff $ lookupEnv "NN_STORMPATH_API_KEY"
        apiKeySecretM <- liftEff $ lookupEnv "NN_STORMPATH_API_SECRET"
        appHrefM      <- liftEff $ lookupEnv "NN_STORMPATH_APPLICATION_HREF"
        case apiKeyIDM, apiKeySecretM, appHrefM of
            Just apiKeyID, Just apiKeySecret, Just appHref -> do
                Stormpath.newAPIKey apiKeyID apiKeySecret
                # Stormpath.newClient
                >>= Stormpath.getApplication `flip` appHref
            _, _, _-> liftEff $ exit 1

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
        server <- createServer $ nodeHandler $ handle db stormpath
        listen server {hostname: "localhost", port: 1337, backlog: Nothing} (pure unit)

handle
    :: ∀ eff
     . Pool
    -> Stormpath.Application
    -> Request
    -> Aff (fs :: FS, uuid :: GENUUID, postgreSQL :: POSTGRESQL, stormpath :: STORMPATH | eff) Response
handle db stormpath req =
    case unwrap req.method, List.fromFoldable (String.split (String.Pattern "/") req.path) of
        "GET",  "" : "" : Nil                                                   -> static "text/html" "index.html"
        "GET",  "" : "output" : "nn.js" : Nil                                   -> static "application/javascript" "output/nn.js"
        "GET",  "" : "output" : "nn.css" : Nil                                  -> static "text/css" "output/nn.css"
        method, "" : "api" : "v1" : "files" : fileID : "vertices" : path ->
            withConnection db (\conn ->
                runMaybeT $
                    runVertexDSLAuthorizationDB conn (UserID <$> getCookie "session" req) $
                        handleVertexAPI (FileID fileID) method path req)
            <#> case _ of
                Just res -> res
                Nothing -> error 403
        "POST", "" : "api" : "v1" : "session" : Nil ->
            case Sexp.fromString (ByteString.toString req.body UTF8) >>= Sexp.fromSexp of
                Just (username /\ password) -> do
                    account <- Stormpath.authenticateAccount stormpath username password
                    let accountHref = Stormpath.accountHref account
                    pure $ setCookie "session" accountHref (error 200)
                Nothing -> pure $ error 400
        _, _ -> pure $ error 404

static :: ∀ eff. String -> String -> Aff (fs :: FS | eff) Response
static mime path = do
    contents <- liftEff' $ ByteString.unsafeFreeze <$> readFile path
    pure { status: {code: 200, message: "OK"}
         , headers: Map.singleton (CaseInsensitiveString "content-type") mime
         , body: contents
         }

error :: Int -> Response
error code =
    { status: {code, message: ""}
    , headers: Map.empty :: Map CaseInsensitiveString String
    , body: ByteString.empty
    }

runVertexDSLAuthorizationDB
    :: ∀ eff
     . Connection
    -> Maybe UserID
    -> VertexDSL
    ~> MaybeT (Aff (postgreSQL :: POSTGRESQL, uuid :: GENUUID | eff))
runVertexDSLAuthorizationDB conn userID = foldFree go
    where
    go :: VertexDSLF ~> MaybeT (Aff (postgreSQL :: POSTGRESQL, uuid :: GENUUID | eff))
    go action = do
        flip runReaderT userID $
            runAuthorizationDSL conn $
                Vertex.DSL.Interpret.Authorization.runVertexDSLF action
        lift $ Vertex.DSL.Interpret.DB.runVertexDSLF conn action

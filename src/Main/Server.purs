module Main.Server
( main
) where

import Data.Map as Map
import Network.HTTP.Message (Request, Response)
import Network.HTTP.Node (nodeHandler)
import Node.HTTP (createServer, listen)
import NN.Prelude

main = do
    server <- createServer $ nodeHandler handler
    listen server {hostname: "localhost", port: 1337, backlog: Nothing} (pure unit)

handler :: forall eff. Request eff -> Aff eff (Response eff)
handler req = pure
    { status: {code: 200, message: "OK"}
    , headers: Map.singleton (CaseInsensitiveString "Content-Type") "text/html"
    , body: pure unit
    }

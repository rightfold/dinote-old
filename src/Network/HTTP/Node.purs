module Network.HTTP.Node
( nodeHandler
) where

import Control.Coroutine (($$), consumer, runProcess)
import Control.Monad.Aff (makeAff)
import Data.Map as Map
import Data.StrMap as StrMap
import Network.HTTP.Message (Request, Response)
import Node.HTTP (HTTP)
import Node.HTTP as N
import Node.Stream as Stream
import NN.Prelude

nodeHandler
    :: ∀ eff
     . (Request (http :: HTTP | eff) -> Aff (http :: HTTP | eff) (Response (http :: HTTP | eff)))
    -> (N.Request -> N.Response -> Eff (http :: HTTP | eff) Unit)
nodeHandler h nReq nRes =
    void $ runAff traceAnyA (const $ pure unit) do
        res <- h {method, path, headers, body}
        liftEff $ N.setStatusCode nRes res.status.code
        liftEff $ N.setStatusMessage nRes res.status.message
        for_ (Map.toList res.headers) \(Tuple (CaseInsensitiveString k) v) ->
            liftEff $ N.setHeader nRes k v
        let nResBody = N.responseAsStream nRes
        runProcess $ res.body $$ consumer \bodyPart -> do
            makeAff \_ ok -> void $ Stream.write nResBody bodyPart (ok unit)
            pure Nothing
        makeAff \_ ok -> Stream.end nResBody (ok unit)
    where
    method = CaseInsensitiveString (N.requestMethod nReq)
    path = N.requestURL nReq
    headers =
        N.requestHeaders nReq
        # StrMap.toList
        # map (\(Tuple k v) -> Tuple (CaseInsensitiveString k) v)
        # Map.fromFoldable
    body = pure unit -- TODO: request body

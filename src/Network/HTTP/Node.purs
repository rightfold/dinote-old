module Network.HTTP.Node
( nodeHandler
) where

import Control.Monad.Aff (makeAff, runAff)
import Data.ByteString (ByteString)
import Data.ByteString as ByteString
import Data.Map as Map
import Data.String.CaseInsensitive (CaseInsensitiveString(..))
import Data.StrMap as StrMap
import Network.HTTP.Message (Request, Response)
import Node.HTTP (HTTP)
import Node.HTTP as N
import Node.Stream as Stream
import NN.Prelude

nodeHandler
    :: ∀ eff
     . (Request -> Aff (http :: HTTP | eff) Response)
    -> (N.Request -> N.Response -> Eff (http :: HTTP | eff) Unit)
nodeHandler h nReq nRes =
    void $ runAff traceAnyA (const $ pure unit) do
        res <- h <<< {method, path, headers, body: _} =<< body
        liftEff $ N.setStatusCode nRes res.status.code
        liftEff $ N.setStatusMessage nRes res.status.message
        for_ (Map.toList res.headers) \(Tuple (CaseInsensitiveString k) v) ->
            liftEff $ N.setHeader nRes k v
        let nResBody = N.responseAsStream nRes
        makeAff \_ ok -> void $ Stream.write (N.responseAsStream nRes) (ByteString.unsafeThaw res.body) (ok unit)
        makeAff \_ ok -> Stream.end nResBody (ok unit)
    where
    method = CaseInsensitiveString (N.requestMethod nReq)
    path = N.requestURL nReq
    headers =
        N.requestHeaders nReq
        # StrMap.toList
        # map (\(Tuple k v) -> Tuple (CaseInsensitiveString k) v)
        # Map.fromFoldable
    body = exhaustStream $ N.requestAsStream nReq

foreign import exhaustStream
    :: ∀ r eff
     . Stream.Readable r eff
    -> Aff eff ByteString

module NN.Server.Web
( ok
, notFound
, forbidden
, badRequest

, interact
) where

import Data.ByteString as ByteString
import Data.Map (Map)
import Data.Map as Map
import Data.Sexp (class FromSexp, fromSexp, class ToSexp, toSexp)
import Data.Sexp as Sexp
import Data.String.CaseInsensitive (CaseInsensitiveString)
import Network.HTTP.Message (Request, Response)
import NN.Prelude
import Node.Encoding (Encoding(UTF8))

ok :: ∀ a. (ToSexp a) => a -> Response
ok value =
    { status: {code: 200, message: "OK"}
    , headers: Map.empty :: Map CaseInsensitiveString String
    , body: ByteString.fromString (Sexp.toString (toSexp value)) UTF8
    }

notFound :: Response
notFound =
    { status: {code: 404, message: "Not Found"}
    , headers: Map.empty :: Map CaseInsensitiveString String
    , body: ByteString.empty
    }

forbidden :: Response
forbidden =
    { status: {code: 403, message: "Forbidden"}
    , headers: Map.empty :: Map CaseInsensitiveString String
    , body: ByteString.empty
    }

badRequest :: Response
badRequest =
    { status: {code: 400, message: "Bad Request"}
    , headers: Map.empty :: Map CaseInsensitiveString String
    , body: ByteString.empty
    }

interact :: ∀ f a. (Applicative f, FromSexp a) => (a -> f Response) -> Request -> f Response
interact k req = maybe (pure badRequest) k $ fromSexp =<< Sexp.fromString body
    where body = ByteString.toString req.body UTF8

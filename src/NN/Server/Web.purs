module NN.Server.Web
( ok
, notFound
, forbidden
) where

import Data.ByteString as ByteString
import Data.Map (Map)
import Data.Map as Map
import Data.Sexp (class ToSexp, toSexp)
import Data.Sexp as Sexp
import Data.String.CaseInsensitive (CaseInsensitiveString)
import Network.HTTP.Message (Response)
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

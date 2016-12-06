module Network.HTTP.Message
( Request
, Response
) where

import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.String.CaseInsensitive (CaseInsensitiveString)

type Request =
    { method :: CaseInsensitiveString
    , path :: String
    , headers :: Map CaseInsensitiveString String
    , body :: ByteString
    }

type Response =
    { status :: {code :: Int, message :: String}
    , headers :: Map CaseInsensitiveString String
    , body :: ByteString
    }

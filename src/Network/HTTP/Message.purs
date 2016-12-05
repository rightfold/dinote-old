module Network.HTTP.Message
( Request
, Response
) where

import Control.Monad.Aff (Aff)
import Control.Coroutine (Producer)
import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.String.CaseInsensitive (CaseInsensitiveString)
import Prelude

type Request eff =
    { method :: CaseInsensitiveString
    , path :: String
    , headers :: Map CaseInsensitiveString String
    , body :: Producer ByteString (Aff eff) Unit
    }

type Response eff =
    { status :: {code :: Int, message :: String}
    , headers :: Map CaseInsensitiveString String
    , body :: Producer ByteString (Aff eff) Unit
    }

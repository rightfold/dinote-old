module Network.HTTP.Message
( Request
, Response
) where

import Control.Monad.Aff (Aff)
import Control.Coroutine (Producer)
import Data.Map (Map)
import Data.String.CaseInsensitive (CaseInsensitiveString)
import Node.Buffer (Buffer)
import Prelude

type Request eff =
    { method :: CaseInsensitiveString
    , headers :: Map CaseInsensitiveString String
    , body :: Producer Buffer (Aff eff) Unit
    }

type Response eff =
    { status :: {code :: Int, message :: String}
    , headers :: Map CaseInsensitiveString String
    , body :: Producer Buffer (Aff eff) Unit
    }

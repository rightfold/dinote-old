module Network.HTTP.Cookie
( getCookie
, setCookie
) where

import Data.Map as Map
import Data.StrMap (StrMap)
import Data.StrMap as StrMap
import Network.HTTP.Message (Request, Response)
import NN.Prelude

getCookie :: String -> Request -> Maybe String
getCookie key {headers} =
    Map.lookup (wrap "Cookie") headers
    <#> parse
    >>= StrMap.lookup key

setCookie :: String -> String -> Response -> Response
setCookie key value res =
    res { headers = Map.alter go (wrap "Set-Cookie") res.headers }
    where
    go = maybe "" (_ <> ", ") >>> (_ <> serialize key value) >>> Just

foreign import parse :: String -> StrMap String
foreign import serialize :: String -> String -> String

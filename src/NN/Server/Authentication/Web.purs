module NN.Server.Authentication.Web
( handleAuthenticationAPI
, handleAuthenticate
) where

import Network.HTTP.Cookie (setCookie)
import Network.HTTP.Message (Request, Response)
import NN.Prelude
import NN.Server.Authentication.DSL (authenticate, AuthenticationDSL)
import NN.Server.Web as Web

handleAuthenticationAPI :: String -> List String -> Request -> AuthenticationDSL Response
handleAuthenticationAPI method path req = case method, path of
    "POST", Nil -> handleAuthenticate req
    _, _ -> pure Web.notFound

handleAuthenticate :: Request -> AuthenticationDSL Response
handleAuthenticate = Web.interact \(username /\ password) ->
    authenticate username password <#> case _ of
        Just userID -> setCookie "session" (unwrap userID) (Web.ok userID)
        Nothing -> Web.notFound

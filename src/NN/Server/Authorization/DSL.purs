module NN.Server.Authorization.DSL
( AuthorizationDSL
, AuthorizationDSLF
, verifyAuthorizedForFile
) where

import Control.Monad.Free (Free, liftF)
import NN.File (FileID)
import NN.Prelude

type AuthorizationDSL = Free AuthorizationDSLF

data AuthorizationDSLF a
    = VerifyAuthorizedForFile FileID a

verifyAuthorizedForFile :: FileID -> AuthorizationDSL Unit
verifyAuthorizedForFile = liftF <<< VerifyAuthorizedForFile `flip` unit

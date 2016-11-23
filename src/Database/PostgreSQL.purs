module Database.PostgreSQL
( POSTGRESQL
, PoolConfiguration
, Pool
, Connection
, newPool
, withConnection
) where

import NN.Prelude

foreign import data POSTGRESQL :: !

type PoolConfiguration =
    { user              :: String
    , password          :: String
    , host              :: String
    , port              :: Int
    , database          :: String
    , max               :: Int
    , idleTimeoutMillis :: Int
    }

foreign import data Pool :: *

foreign import data Connection :: *

foreign import newPool
    :: ∀ eff
     . PoolConfiguration
    -> Aff (postgreSQL :: POSTGRESQL | eff) Pool

foreign import withConnection
    :: ∀ eff a
     . Pool
    -> (Connection -> Aff (postgreSQL :: POSTGRESQL | eff) a)
    -> Aff (postgreSQL :: POSTGRESQL | eff) a

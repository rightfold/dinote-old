module Database.PostgreSQL
( POSTGRESQL
, PoolConfiguration
, Pool
, Connection
, class ToSQLRow
, class FromSQLRow
, class ToSQLValue
, class FromSQLValue
, toSQLRow
, fromSQLRow
, toSQLValue
, fromSQLValue
, newPool
, withConnection
, execute
, query
) where

import Control.Monad.Except (runExcept)
import Data.Foreign (Foreign, readArray, readString, toForeign)
import Data.Tuple.Nested (tuple1, tuple2, tuple3, tuple4, tuple5)
import NN.Prelude
import Partial.Unsafe (unsafePartial)

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

class ToSQLRow a where
    toSQLRow :: a -> Array Foreign

class FromSQLRow a where
    fromSQLRow :: Array Foreign -> Maybe a

class ToSQLValue a where
    toSQLValue :: a -> Foreign

class FromSQLValue a where
    fromSQLValue :: Foreign -> Maybe a

instance toSQLRowUnit :: ToSQLRow Unit where
    toSQLRow _ = []

instance toSQLRowTuple1 :: (ToSQLValue a) => ToSQLRow (Tuple a Unit) where
    toSQLRow (a /\ _) = [toSQLValue a]

instance toSQLRowTuple2 :: (ToSQLValue a, ToSQLValue b) => ToSQLRow (Tuple a (Tuple b Unit)) where
    toSQLRow (a /\ b /\ _) = [toSQLValue a, toSQLValue b]

instance toSQLRowTuple3 :: (ToSQLValue a, ToSQLValue b, ToSQLValue c) => ToSQLRow (Tuple a (Tuple b (Tuple c Unit))) where
    toSQLRow (a /\ b /\ c /\ _) = [toSQLValue a, toSQLValue b, toSQLValue c]

instance toSQLRowTuple4 :: (ToSQLValue a, ToSQLValue b, ToSQLValue c, ToSQLValue d) => ToSQLRow (Tuple a (Tuple b (Tuple c (Tuple d Unit)))) where
    toSQLRow (a /\ b /\ c /\ d /\ _) = [toSQLValue a, toSQLValue b, toSQLValue c, toSQLValue d]

instance toSQLRowTuple5 :: (ToSQLValue a, ToSQLValue b, ToSQLValue c, ToSQLValue d, ToSQLValue e) => ToSQLRow (Tuple a (Tuple b (Tuple c (Tuple d (Tuple e Unit))))) where
    toSQLRow (a /\ b /\ c /\ d /\ e /\ _) = [toSQLValue a, toSQLValue b, toSQLValue c, toSQLValue d, toSQLValue e]

instance fromSQLRowUnit :: FromSQLRow Unit where
    fromSQLRow [] = Just unit
    fromSQLRow _ = Nothing

instance fromSQLRowTuple1 :: (FromSQLValue a) => FromSQLRow (Tuple a Unit) where
    fromSQLRow [a] = tuple1 <$> fromSQLValue a
    fromSQLRow _ = Nothing

instance fromSQLRowTuple2 :: (FromSQLValue a, FromSQLValue b) => FromSQLRow (Tuple a (Tuple b Unit)) where
    fromSQLRow [a, b] = tuple2 <$> fromSQLValue a <*> fromSQLValue b
    fromSQLRow _ = Nothing

instance fromSQLRowTuple3 :: (FromSQLValue a, FromSQLValue b, FromSQLValue c) => FromSQLRow (Tuple a (Tuple b (Tuple c Unit))) where
    fromSQLRow [a, b, c] = tuple3 <$> fromSQLValue a <*> fromSQLValue b <*> fromSQLValue c
    fromSQLRow _ = Nothing

instance fromSQLRowTuple4 :: (FromSQLValue a, FromSQLValue b, FromSQLValue c, FromSQLValue d) => FromSQLRow (Tuple a (Tuple b (Tuple c (Tuple d Unit)))) where
    fromSQLRow [a, b, c, d] = tuple4 <$> fromSQLValue a <*> fromSQLValue b <*> fromSQLValue c <*> fromSQLValue d
    fromSQLRow _ = Nothing

instance fromSQLRowTuple5 :: (FromSQLValue a, FromSQLValue b, FromSQLValue c, FromSQLValue d, FromSQLValue e) => FromSQLRow (Tuple a (Tuple b (Tuple c (Tuple d (Tuple e Unit))))) where
    fromSQLRow [a, b, c, d, e] = tuple5 <$> fromSQLValue a <*> fromSQLValue b <*> fromSQLValue c <*> fromSQLValue d <*> fromSQLValue e
    fromSQLRow _ = Nothing

instance toSQLValueString :: ToSQLValue String where
    toSQLValue = toForeign

instance fromSQLValueString :: FromSQLValue String where
    fromSQLValue = fromRight <<< runExcept <<< readString

instance fromSQLValueArray :: (FromSQLValue a) => FromSQLValue (Array a) where
    fromSQLValue = traverse fromSQLValue <=< fromRight <<< runExcept <<< readArray

foreign import newPool
    :: ∀ eff
     . PoolConfiguration
    -> Aff (postgreSQL :: POSTGRESQL | eff) Pool

foreign import withConnection
    :: ∀ eff a
     . Pool
    -> (Connection -> Aff (postgreSQL :: POSTGRESQL | eff) a)
    -> Aff (postgreSQL :: POSTGRESQL | eff) a

execute
    :: ∀ i eff
     . (ToSQLRow i)
    => Connection
    -> String
    -> i
    -> Aff (postgreSQL :: POSTGRESQL | eff) Unit
execute conn sql values =
    void $ _query conn sql (toSQLRow values)

query
    :: ∀ i o eff
     . (ToSQLRow i, FromSQLRow o)
    => Connection
    -> String
    -> i
    -> Aff (postgreSQL :: POSTGRESQL | eff) (Array o)
query conn sql values =
    _query conn sql (toSQLRow values)
    <#> map (unsafePartial fromJust <<< fromSQLRow)

foreign import _query
    :: ∀ eff
     . Connection
    -> String
    -> Array Foreign
    -> Aff (postgreSQL :: POSTGRESQL | eff) (Array (Array Foreign))

fromRight :: ∀ a b. Either a b -> Maybe b
fromRight (Left _) = Nothing
fromRight (Right a) = Just a

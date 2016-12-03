module NN.File
( FileID(..)
, File(..)
) where

import Data.Generic (class Generic, gShow)
import Data.Sexp (class AsSexp, gFromSexp, gToSexp, class FromSexp, class ToSexp)
import NN.Prelude
import NN.Vertex (VertexID)

newtype FileID = FileID String

derive instance genericFileID :: Generic FileID
derive instance eqFileID :: Eq FileID
derive instance ordFileID :: Ord FileID
instance fromSexpFileID :: FromSexp FileID where fromSexp = gFromSexp
instance toSexpFileID :: ToSexp FileID where toSexp = gToSexp
instance asSexpFileID :: AsSexp FileID
instance showFileID :: Show FileID where show = gShow

data File = File String VertexID

derive instance genericFile :: Generic File
instance showFile :: Show File where show = gShow
instance fromSexpFile :: FromSexp File where fromSexp = gFromSexp
instance toSexpFile :: ToSexp File where toSexp = gToSexp
instance asSexpFile :: AsSexp File

module NN.File
( FileID(..)
, File(..)
) where

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Sexp (class AsSexp, genericFromSexp, genericToSexp, class FromSexp, class ToSexp)
import NN.Prelude
import NN.Vertex (VertexID)

newtype FileID = FileID String

derive instance genericFileID :: Generic FileID _
derive instance eqFileID :: Eq FileID
derive instance ordFileID :: Ord FileID
instance fromSexpFileID :: FromSexp FileID where fromSexp = genericFromSexp
instance toSexpFileID :: ToSexp FileID where toSexp = genericToSexp
instance asSexpFileID :: AsSexp FileID
instance showFileID :: Show FileID where show = genericShow

data File = File String VertexID

derive instance genericFile :: Generic File _
instance fromSexpFile :: FromSexp File where fromSexp = genericFromSexp
instance toSexpFile :: ToSexp File where toSexp = genericToSexp
instance asSexpFile :: AsSexp File
instance showFile :: Show File where show = genericShow

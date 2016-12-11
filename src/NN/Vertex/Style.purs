module NN.Vertex.Style
( Style(..)
) where

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Sexp (class AsSexp, genericFromSexp, genericToSexp, class FromSexp, class ToSexp)
import NN.Prelude

data Style = Normal | Dimmed | Grass | Ocean | Peachpuff | HotdogStand

derive instance genericStyle :: Generic Style _
derive instance eqStyle :: Eq Style
derive instance ordStyle :: Ord Style
instance fromSexpStyle :: FromSexp Style where fromSexp = genericFromSexp
instance toSexpStyle :: ToSexp Style where toSexp = genericToSexp
instance asSexpStyle :: AsSexp Style
instance showStyle :: Show Style where show = genericShow

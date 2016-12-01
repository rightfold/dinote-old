module NN.Vertex.Style
( Style(..)
) where

import Data.Generic (class Generic, gShow)
import NN.Prelude

data Style = Normal | Dimmed | Grass | Ocean | Peachpuff | HotdogStand

derive instance genericStyle :: Generic Style
derive instance eqStyle :: Eq Style
derive instance ordStyle :: Ord Style
instance showStyle :: Show Style where show = gShow

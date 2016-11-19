module NN.Vertex.Style
( Style(..)
, styleClass
) where

import Halogen.HTML.Core (ClassName(..))

data Style = Normal | Dimmed | Grass | Ocean | Peachpuff | HotdogStand

styleClass :: Style -> ClassName
styleClass Normal = ClassName "-style-normal"
styleClass Dimmed = ClassName "-style-dimmed"
styleClass Grass = ClassName "-style-grass"
styleClass Ocean = ClassName "-style-ocean"
styleClass Peachpuff = ClassName "-style-peachpuff"
styleClass HotdogStand = ClassName "-style-hotdog-stand"

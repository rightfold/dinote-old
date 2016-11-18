module NN.Vertex.Style
( Style(..)
, styleClass
) where

import Halogen.HTML.Core (ClassName(..))

data Style = Normal | Dimmed | Grass | Warning | Peachpuff | HotdogStand

styleClass :: Style -> ClassName
styleClass Normal = ClassName "-style-normal"
styleClass Dimmed = ClassName "-style-dimmed"
styleClass Grass = ClassName "-style-grass"
styleClass Warning = ClassName "-style-warning"
styleClass Peachpuff = ClassName "-style-peachpuff"
styleClass HotdogStand = ClassName "-style-hotdog-stand"

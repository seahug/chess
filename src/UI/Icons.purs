module Chess.UI.Icons where

import Chess.UI.Css as Css
import Halogen.HTML (ClassName, HTML)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

icon :: forall w i. ClassName -> HTML w i
icon i = HH.i [ HP.classes [ Css.icon, i ] ] []

empty :: forall w i. HTML w i
empty = HH.i [ HP.classes [ Css.icon ] ] []

settings :: forall w i. HTML w i
settings = icon Css.settings

check :: forall w i. HTML w i
check = icon Css.check


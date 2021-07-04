module Chess.UI.Settings where

import Prelude

import AppAction (AppAction(..))
import Data.Maybe (Maybe(..))
import Chess.UI.Css as Css
import Chess.UI.RenderText (renderText)
import Chess.UI.UsernameInput as UsernameInput
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Version (version)

renderEmpty = HH.div
  [ HP.classes
    [ Css.settingsMenu
    , Css.collapsed
    ]
  ]
  []

render cs@{ showMenu } = HH.div
  [ HP.classes
    [ Css.settingsMenu
    , if showMenu
      then Css.showing
      else Css.collapsed
    ]
  ]
  [ HH.div
    [ HP.class_ Css.backButtonContainer ]
    [ HH.button
      [ HE.onClick \_ -> Just $ ToggleMenu
      , HP.class_ Css.backButton
      ]
      [ HH.text "Back" ]
    ]
  , renderText version
  , UsernameInput.render { onInput: WriteUsername, state: cs }
  ]

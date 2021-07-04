module AppAction where

import AppState (AppState)
import Data.Lens.Lens (Lens')
import Web.Event.Event (Event)

data AppAction
  = Initialize
  | ToggleMenu
  | WriteUsername String
  | Write (Lens' AppState String) String
  | SendMessage
  | ReceiveRemoteMessage Event
  | ReceiveLocalMessage Event

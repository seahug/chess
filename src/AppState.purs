module AppState where

import Data.HashMap (HashMap)
import Data.HashMap as HashMap
import Data.Lens.Lens (Lens', Lens)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Chess.Capability.Broadcast (Broadcaster)
import Message (RemoteMessage)

type AppState =
  { connectionCount :: Int
  , id :: String
  , username :: String
  , usernames :: HashMap String String
  , message :: String
  , messages :: Array RemoteMessage
  , maybeBroadcaster :: Maybe Broadcaster
  , roomCode :: String
  , showMenu :: Boolean
  }

_id :: Lens' AppState String
_id = prop (SProxy :: SProxy "id")
_messages :: Lens' AppState (Array RemoteMessage)
_messages = prop (SProxy :: SProxy "messages")
_message :: Lens' AppState String
_message = prop (SProxy :: SProxy "message")
_connectionCount :: Lens' AppState Int
_connectionCount = prop (SProxy :: SProxy "connectionCount")
_usernames :: Lens' AppState (HashMap String String)
_usernames = prop (SProxy :: SProxy "usernames")
_username :: Lens' AppState String
_username = prop (SProxy :: SProxy "username")
_maybeBroadcaster :: Lens' AppState (Maybe Broadcaster)
_maybeBroadcaster = prop (SProxy :: SProxy "maybeBroadcaster")
_showMenu
  :: forall a b r
  . Lens { showMenu :: a | r } { showMenu :: b | r } a b
_showMenu = prop (SProxy :: SProxy "showMenu")

globalRoomCode :: String
globalRoomCode = "chess"

newApp :: AppState
newApp =
  { connectionCount: 0
  , id: ""
  , username: ""
  , usernames: HashMap.empty
  , message: ""
  , messages: []
  , maybeBroadcaster: Nothing
  , roomCode: globalRoomCode
  , showMenu: false
  }

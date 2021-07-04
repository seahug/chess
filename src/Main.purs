module Main where

import Prelude

import AppAction (AppAction(..))
import AppState (_connectionCount, _id, _maybeBroadcaster, _message, _messages, _showMenu, _username, _usernames, newApp)
import Data.Array (take)
import Data.Bifunctor (rmap)
import Data.Either (Either(..))
import Data.HashMap as HashMap
import Data.Lens.Getter (view)
import Data.Lens.Prism (review)
import Data.Lens.Setter (over, set, (%~), (.~))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Chess.AppM (runAppM)
import Chess.Capability.Broadcast (broadcast, maybeCreateBroadcaster)
import Chess.Capability.GenUuid (genUuid)
import Chess.Capability.Log (error, log)
import Chess.Capability.Random (randomElement)
import Chess.Capability.Storage (load, save)
import Chess.Capability.WireCodec (readWire, writeWire)
import Chess.UI.Chat as Chat
import Chess.UI.Css as Css
import Chess.UI.Icons as Icons
import Chess.UI.Settings as Settings
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Emojis (emojis)
import FFI as FFI
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Message (LocalMessage(..), RemoteMessage(..), WireEnvelope)
import Message as Message
import Util ((:~))
import Web.Event.Event (EventType(..))

remoteMessageTarget :: String
remoteMessageTarget = "remote-message-target"

localMessageTarget :: String
localMessageTarget = "local-message-target"

uuidKey :: String
uuidKey = "player-id"

usernameKey :: String
usernameKey = "username"

main :: Effect Unit
main = launchAff_ $ do
  liftEffect $ FFI.registerServiceWorker
  HA.awaitLoad
  body <- HA.awaitBody
  runUI (root) unit body

root = H.hoist (runAppM {}) component

component =
  H.mkComponent { eval, initialState, render }
  where
  eval = H.mkEval H.defaultEval
    { handleAction = handleAction
    , initialize = Just Initialize
    }
  initialState _ = newApp

render state = HH.main_ $
  [ HH.div
    [ HP.id_ $ remoteMessageTarget
    , HE.handler (EventType "purescript") (Just <<< ReceiveRemoteMessage)
    ]
    []
  , HH.div
    [ HP.id_ $ localMessageTarget
    , HE.handler
      (EventType "purescript")
      (Just <<< ReceiveLocalMessage)
    ]
    []
  , if state.showMenu
    then Settings.render state
    else Settings.renderEmpty
  , renderSettingsButton
  , HH.h1_ [ HH.text "Chess" ]
  , Chat.render
    { sendEvent: SendMessage
    , onInput: Write _message
    , state
    }
  , HH.i
    [ HP.class_ Css.connections ]
    [ HH.text $ show state.connectionCount ]
  ]

renderSettingsButton = HH.button
  [ HP.class_ Css.settingsButton
  , HH.attr (H.AttrName "aria-label") "Settings"
  , HE.onClick \_ -> Just ToggleMenu
  ]
  [ Icons.settings ]

handleAction = case _ of
  Initialize -> do
    eUuid <- load uuidKey
    uuid <- case eUuid of
      Left e -> do
        log "no existing uuid found, generating a new one"
        uuid <- genUuid
        save uuidKey uuid
        pure $ show uuid
      Right uuid -> pure uuid

    eUsername <- load usernameKey
    username <- case eUsername of
      Left e -> do
        log "no existing username found, using default"
        emoji <- fromMaybe ":)" <$> randomElement emojis
        pure $ emoji <> "lurker" <> emoji
      Right u -> pure u

    roomCode <- H.gets _.roomCode

    maybeBroadcaster <- maybeCreateBroadcaster
      roomCode remoteMessageTarget localMessageTarget

    H.modify_ $ (_id .~ uuid)
      >>> (_username .~ username)
      >>> (_usernames %~ HashMap.insert uuid username)
      >>> (_maybeBroadcaster .~ maybeBroadcaster)

  ToggleMenu -> H.modify_ $ _showMenu %~ not

  WriteUsername username -> do
    { id, usernames } <- H.get
    save "username" username
    H.modify_ $ set _username username
      <<< over _usernames (HashMap.insert id username)
    sendMessage $ UsernameMessage { username, id }
  Write lens value -> H.modify_ $ set lens value
  SendMessage -> sendChatMessage
  ReceiveLocalMessage customEvent -> do
    let localMessage = FFI.detail customEvent
    case localMessage of
      SeenMessage address -> do
        log $ "I see you: " <> address
        { username, id } <- H.get
        sendMessage $ UsernameMessage { username, id }
      ConnectionsMessage count ->
        H.modify_ $ set _connectionCount count

  ReceiveRemoteMessage customEvent -> do
    let detail = FFI.detail customEvent
    (eWireEnvelope :: Either String WireEnvelope) <- readWire detail
    let
      eMessage = (
        rmap
        (review Message._toWire))
        <$> eWireEnvelope
    case eMessage of
      Left e -> error $ "problem receiving message: " <> e
      Right (Tuple _ msg) -> do
        case msg of
          UsernameMessage { username, id } -> do
            log $ "username incoming: " <> username
            H.modify_ $ over _usernames
              $ HashMap.insert id username
          ChatMessage { message, username } -> do
            H.modify_
              $ (_messages :~ msg)
              >>> (_messages %~ take 250)
            if message == "PING"
              then do
                H.modify_ $ _message .~ "PONG"
                sendChatMessage
              else pure unit
  where
    sendChatMessage = do
      { id, message } <- H.get
      let chat = ChatMessage { username: id, message }
      H.modify_ $ (_message .~ "") <<< (_messages :~ chat)
      sendMessage chat

    sendMessage message' = do
      let message = view Message._toWire message'
      { roomCode, maybeBroadcaster, id } <- H.get
      let wireEnvelope = Tuple id message

      maybeBroadcaster' <- case maybeBroadcaster of
        Nothing -> maybeCreateBroadcaster
          roomCode remoteMessageTarget localMessageTarget
        Just b -> pure (Just b)

      case maybeBroadcaster' of
        Nothing ->
          log "no broadcaster to send message"
        Just broadcaster -> do
          eString <- writeWire wireEnvelope
          case eString of
            Left e -> error e
            Right string ->
              broadcast broadcaster string


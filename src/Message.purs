module Message where

import Prelude

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.ArrayBuffer.Class (class DecodeArrayBuffer, class DynamicByteLength, class EncodeArrayBuffer, genericByteLength, genericPutArrayBuffer, genericReadArrayBuffer)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens.Fold (preview, (^?))
import Data.Lens.Getter (view)
import Data.Lens.Iso (Iso', iso)
import Data.Lens.Prism (review)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Chess.Data.Wire.Int (WireInt)
import Chess.Data.Wire.Int as Int
import Halogen.HTML (ClassName(..), HTML)
import Halogen.HTML (text) as HH
import Halogen.HTML.Elements (div, span, span_) as HH
import Halogen.HTML.Properties (class_) as HH

type Envelope = { id :: String, message :: RemoteMessage }
type WireEnvelope = Tuple String WireMessage

wrapMessage :: String -> RemoteMessage -> Envelope
wrapMessage id message = { id, message }

data RemoteMessage
  = ChatMessage { username :: String, message :: String }
  | UsernameMessage { username :: String, id :: String }

data LocalMessage
  = SeenMessage String
  | ConnectionsMessage Int

derive instance genericRemoteMessage :: Generic RemoteMessage _
derive instance eqRemoteMessage :: Eq RemoteMessage
instance showRemoteMessage :: Show RemoteMessage where
  show = genericShow
instance encodeJsonRemoteMessage :: EncodeJson RemoteMessage where
  encodeJson = genericEncodeJson
instance decodeJsonRemoteMessage :: DecodeJson RemoteMessage where
  decodeJson = genericDecodeJson

data WireMessage
  = ChatWireMessage (Tuple String String)
  | UsernameWireMessage (Tuple String String)

_toWire :: Iso' RemoteMessage WireMessage
_toWire = iso to from where
  to = case _ of
    ChatMessage { username, message } ->
      ChatWireMessage (Tuple username message)
    UsernameMessage { username, id} ->
      UsernameWireMessage (Tuple username id)
  from = case _ of
    ChatWireMessage (Tuple username message) ->
      ChatMessage { username, message }
    UsernameWireMessage (Tuple username id) ->
      UsernameMessage { username, id}

derive instance genericWireMessage :: Generic WireMessage _
derive instance eqWireMessage :: Eq WireMessage
instance showWireMessage :: Show WireMessage where
  show = genericShow
instance encodeJsonWireMessage :: EncodeJson WireMessage where
  encodeJson = genericEncodeJson
instance decodeJsonWireMessage :: DecodeJson WireMessage where
  decodeJson = genericDecodeJson
instance dynamicByteLengthWireMessage
  :: DynamicByteLength WireMessage where
  byteLength = genericByteLength
instance encodeArrayBuffeWireMessage
  :: EncodeArrayBuffer WireMessage where
  putArrayBuffer = genericPutArrayBuffer
instance decodeArrayBuffeWireMessage
  :: DecodeArrayBuffer WireMessage where
  readArrayBuffer = genericReadArrayBuffer

renderHtml :: forall w i. RemoteMessage -> HTML w i
renderHtml (ChatMessage { username, message }) =
  HH.div
    [ HH.class_ $ ClassName "chat-message" ]
    [ HH.span [ HH.class_ $ ClassName "username" ] [ HH.text username ]
    , HH.text ": "
    , HH.span [ HH.class_ $ ClassName "message" ] [ HH.text message ]
    ]
renderHtml (UsernameMessage { username, id }) =
  HH.div
    [ HH.class_ $ ClassName "username-message" ]
    [ HH.text "("
    , HH.span [ HH.class_ $ ClassName "username" ] [ HH.text $ show username ]
    , HH.text " has ID: "
    , HH.span [ HH.class_ $ ClassName "username" ] [ HH.text $ show id ]
    , HH.text ")"
    ]


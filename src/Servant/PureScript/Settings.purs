{--
  This file contains code copied from the purescript-affjax project from slamdata.
  It is therefore licensed under Apache License version 2.0.
--}
module Servant.PureScript.Settings where

import Prelude
import Data.Argonaut.Core (Json, stringify)
import Data.Argonaut.Decode (JsonDecodeError)
import Data.Argonaut.Decode.Generic (class DecodeRep, genericDecodeJson)
import Data.Argonaut.Encode.Generic (class EncodeRep, genericEncodeJson)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)

foreign import encodeURIComponent :: String -> String

-- encodeJson, decodeJson, toURLPiece have to be wrapped in newtype. See:
-- https://github.com/purescript/purescript/issues/1957

newtype SPSettingsEncodeJson_ = SPSettingsEncodeJson_ (forall a rep. Generic a rep => EncodeRep rep => a -> Json)
newtype SPSettingsDecodeJson_ = SPSettingsDecodeJson_ (forall a rep. Generic a rep => DecodeRep rep => Json -> Either JsonDecodeError a)
newtype SPSettingsToUrlPiece_ = SPSettingsToUrlPiece_ (forall a. ToUrlPiece a => a -> URLPiece)
newtype SPSettingsEncodeHeader_ = SPSettingsEncodeHeader_ (forall a. ToUrlPiece a => a -> URLPiece)

newtype SPSettings_ params = SPSettings_ {
    encodeJson :: SPSettingsEncodeJson_
  , decodeJson :: SPSettingsDecodeJson_
  , toURLPiece :: SPSettingsToUrlPiece_
  , encodeHeader :: SPSettingsEncodeHeader_
  , params :: params
  }

type URLPiece = String

class ToUrlPiece a where
  toURLPiece :: a -> URLPiece

instance toURLPieceString :: ToUrlPiece String where
  toURLPiece = identity

else instance toURLPieceGeneric :: (Generic a rep, EncodeRep rep) => ToUrlPiece a where
  toURLPiece = stringify <<< genericEncodeJson

-- | Just use the robust JSON format.
gDefaultToURLPiece :: forall a. ToUrlPiece a => a -> URLPiece
gDefaultToURLPiece = gDefaultEncodeHeader

-- | Just use the robust JSON format.
gDefaultEncodeHeader :: forall a. ToUrlPiece a => a -> URLPiece
gDefaultEncodeHeader = toURLPiece

-- | Full encoding based on gDefaultToURLPiece
gDefaultEncodeURLPiece :: forall a. ToUrlPiece a => a -> URLPiece
gDefaultEncodeURLPiece = encodeURIComponent <<< gDefaultToURLPiece


defaultSettings :: forall params. params -> SPSettings_ params
defaultSettings params = SPSettings_ {
    encodeJson : SPSettingsEncodeJson_ genericEncodeJson
  , decodeJson : SPSettingsDecodeJson_ genericDecodeJson
  , toURLPiece : SPSettingsToUrlPiece_ gDefaultToURLPiece
  , encodeHeader : SPSettingsEncodeHeader_ gDefaultEncodeHeader
  , params : params
}

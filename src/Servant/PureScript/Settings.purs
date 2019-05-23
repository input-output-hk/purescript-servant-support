{--
  This file contains code copied from the purescript-affjax project from slamdata.
  It is therefore licensed under Apache License version 2.0.
--}
module Servant.PureScript.Settings where

import Data.Generic.Rep (class Generic)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Foreign.Generic (genericEncodeJSON, defaultOptions)
import Foreign.Generic.Class (class GenericEncode)
import Foreign.Generic.Types (Options)
import Prelude (identity, (<<<), ($))
import Servant.PureScript.JsUtils (encodeUriComponent)

-- encodeJSON, decodeJson, toURLPiece have to be wrapped in newtype. See:
-- https://github.com/purescript/purescript/issues/1957

newtype SPSettingsEncodeJson_ = SPSettingsEncodeJson_ Options
derive instance newtypeSPSettingsEncodeJson_ :: Newtype SPSettingsEncodeJson_ _

newtype SPSettingsDecodeJson_ = SPSettingsDecodeJson_ Options
derive instance newtypeSPSettingsDecodeJson_ :: Newtype SPSettingsDecodeJson_ _

newtype SPSettingsToUrlPiece_ = SPSettingsToUrlPiece_ (forall a. ToUrlPiece a => a -> URLPiece)
newtype SPSettingsEncodeHeader_ = SPSettingsEncodeHeader_ (forall a. ToUrlPiece a => a -> URLPiece)

newtype SPSettings_ params = SPSettings_
  { encodeJson :: SPSettingsEncodeJson_
  , decodeJson :: SPSettingsDecodeJson_
  , toURLPiece :: SPSettingsToUrlPiece_
  , encodeHeader :: SPSettingsEncodeHeader_
  , params :: params
  }

derive instance newtypeSPSettings_ :: Newtype (SPSettings_ params) _

type URLPiece = String

class ToUrlPiece a where
  toUrlPiece :: a -> URLPiece

instance stringToUrlPiece :: ToUrlPiece String where
  toUrlPiece = identity

else instance genericRepToUrlPiece :: (Generic a rep, GenericEncode rep) => ToUrlPiece a where
  toUrlPiece = genericEncodeJSON $ defaultOptions { unwrapSingleConstructors = true }

-- | Just use the robust JSON format.
gDefaultToURLPiece :: forall a. ToUrlPiece a => a -> URLPiece
gDefaultToURLPiece = gDefaultEncodeHeader

-- | Just use the robust JSON format.
gDefaultEncodeHeader :: forall a. ToUrlPiece a => a -> URLPiece
gDefaultEncodeHeader = toUrlPiece

-- | Full encoding based on gDefaultToURLPiece
gDefaultEncodeURLPiece :: forall a. ToUrlPiece a => a -> URLPiece
gDefaultEncodeURLPiece = encodeUriComponent <<< gDefaultToURLPiece

defaultSettings :: forall params. params -> SPSettings_ params
defaultSettings params = SPSettings_
  { encodeJson : SPSettingsEncodeJson_ defaultOptions
  , decodeJson : SPSettingsDecodeJson_ defaultOptions
  , toURLPiece : SPSettingsToUrlPiece_ gDefaultToURLPiece
  , encodeHeader : SPSettingsEncodeHeader_ gDefaultEncodeHeader
  , params : params
  }

_encodeJson :: forall params. Lens' (SPSettings_ params) Options
_encodeJson = _Newtype <<< prop (SProxy :: SProxy "encodeJson") <<< _Newtype

_decodeJson :: forall params. Lens' (SPSettings_ params) Options
_decodeJson = _Newtype <<< prop (SProxy :: SProxy "decodeJson") <<< _Newtype

_params :: forall params. Lens' (SPSettings_ params) params
_params = _Newtype <<< prop (SProxy :: SProxy "params")

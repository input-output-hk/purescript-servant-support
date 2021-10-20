module Servant.PureScript where

import Prelude
import Affjax (Error, Request, Response)
import Data.Argonaut.Core (Json, stringify)
import Data.Argonaut.Decode (JsonDecodeError)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)

class ToURLPiece a where
  toURLPiece :: a -> String

instance toURLPieceString :: ToURLPiece String where
  toURLPiece = identity
else instance toURLPieceAny :: (EncodeJson a) => ToURLPiece a where
  toURLPiece = stringify <<< encodeJson

type AjaxError
  = { request :: Request Json
    , description :: ErrorDescription
    }

data ErrorDescription
  = UnexpectedHTTPStatus (Response Json)
  | DecodingError JsonDecodeError
  | ConnectingError Error

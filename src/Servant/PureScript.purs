module Servant.PureScript (class ToURLPiece, toURLPiece, AjaxError, ErrorDescription(..), printAjaxError) where

import Prelude

import Affjax (Error, Request, Response, printError)
import Data.Argonaut.Core (Json, stringify, stringifyWithIndent)
import Data.Argonaut.Decode (JsonDecodeError, printJsonDecodeError)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Array ((:))
import Data.Newtype (unwrap)
import Data.String (Pattern(..), joinWith, split)

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

printAjaxError :: AjaxError -> String
printAjaxError { description } =
  joinWith "\n" $
    "Error making web request:"
    : ( ("  " <> _)
          <$> split (Pattern "\n") case description of
              UnexpectedHTTPStatus response -> printUnexpectedHTTPStatus response
              DecodingError error -> printJsonDecodeError error
              ConnectingError error -> printError error
      )

printUnexpectedHTTPStatus :: Response Json -> String
printUnexpectedHTTPStatus response =
  joinWith "\n"
    [ "Status code: " <> show (unwrap response.status)
    , "Status text: " <> response.statusText
    , "Content: " <> stringifyWithIndent 2 response.body
    ]

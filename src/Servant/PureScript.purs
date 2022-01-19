module Servant.PureScript
  ( class ToURLPiece
  , toURLPiece
  , AjaxError
  , ErrorDescription(..)
  , printAjaxError
  ) where

import Prelude

import Affjax (Error, Request, Response, printError)
import Data.Argonaut
  ( Json
  , JsonDecodeError
  , printJsonDecodeError
  , stringify
  , stringifyWithIndent
  )
import Data.Array ((:))
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Int (decimal)
import Data.Int as Int
import Data.Newtype (unwrap)
import Data.Number.Format as Number
import Data.String (Pattern(..), joinWith, split)
import Data.UUID (UUID)
import Data.UUID as UUID

foreign import encodeURIComponent :: String -> String

class ToURLPiece a where
  toURLPiece :: a -> String

instance ToURLPiece String where
  toURLPiece = encodeURIComponent

instance ToURLPiece Int where
  toURLPiece = toURLPiece <<< Int.toStringAs decimal

instance ToURLPiece Number where
  toURLPiece = toURLPiece <<< Number.toString

instance ToURLPiece UUID where
  toURLPiece = toURLPiece <<< UUID.toString

instance ToURLPiece BigInt where
  toURLPiece = toURLPiece <<< BigInt.toString

instance ToURLPiece Json where
  toURLPiece = toURLPiece <<< stringify

type AjaxError =
  { request :: Request Json
  , description :: ErrorDescription
  }

data ErrorDescription
  = UnexpectedHTTPStatus (Response Json)
  | DecodingError JsonDecodeError
  | ConnectingError Error

printAjaxError :: AjaxError -> String
printAjaxError { description } =
  joinWith "\n" $
    "Error making web request:" :
      ( ("  " <> _)
          <$> split (Pattern "\n") case description of
            UnexpectedHTTPStatus response -> printUnexpectedHTTPStatus
              response
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

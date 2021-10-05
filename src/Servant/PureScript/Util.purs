module Servant.PureScript.Util where

import Prelude
import Control.Monad.Error.Class (class MonadError, throwError)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (JsonDecodeError)
import Data.Argonaut.Encode.Generic (class EncodeRep)
import Data.Argonaut.Parser (jsonParser)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (intercalate)
import Data.Generic.Rep (class Generic)
import Affjax (Response)
import Affjax.StatusCode (StatusCode(..))
import Servant.PureScript.Affjax (makeAjaxError, AjaxError, ErrorDescription(DecodingError, ParsingError, UnexpectedHTTPStatus), AjaxRequest)
import Servant.PureScript.Settings (SPSettings_(SPSettings_), SPSettingsToUrlPiece_(SPSettingsToUrlPiece_), SPSettingsEncodeHeader_(SPSettingsEncodeHeader_))

-- | Get the result of a request.
-- |
-- | Reports an error if status code is non success or decoding fails. The taken AjaxRequest is only for error reporting.
getResult
  :: forall a m rep
   . Generic a rep
  => MonadError AjaxError m
  => AjaxRequest
  -> (Json -> Either JsonDecodeError a)
  -> Response String -> m a
getResult req' decode resp = do
  let stCode = case resp.status of StatusCode code -> code
  fVal <- if stCode >= 200 && stCode < 300
            then pure resp.body
            else throwError $ makeAjaxError req' (UnexpectedHTTPStatus resp)
  jVal <- throwLeft <<< lmap (reportRequestError req' ParsingError) <<< jsonParser $ fVal
  throwLeft <<< lmap (reportRequestError req' DecodingError) <<< decode $ jVal

throwLeft :: forall a e m. MonadError e m => Either e a -> m a
throwLeft (Left e) = throwError e
throwLeft (Right a) = pure a


encodeListQuery :: forall a b rep. Generic a rep => EncodeRep rep => SPSettings_ b -> String -> Array a -> String
encodeListQuery opts'@(SPSettings_ opts) fName = intercalate "&" <<< map (encodeQueryItem opts' fName)

-- | The given name is assumed to be already escaped.
encodeQueryItem :: forall a b rep. Generic a rep => EncodeRep rep => SPSettings_ b -> String -> a -> String
encodeQueryItem opts'@(SPSettings_ opts) fName val = fName <> "=" <> encodeURLPiece opts' val

-- | Call opts.toURLPiece and encode the resulting string with encodeURIComponent.
encodeURLPiece :: forall a params rep. Generic a rep => EncodeRep rep => SPSettings_ params -> a -> String
encodeURLPiece (SPSettings_ opts) = case opts.toURLPiece of SPSettingsToUrlPiece_ f -> f

encodeHeader :: forall a params rep. Generic a rep => EncodeRep rep => SPSettings_ params -> a -> String
encodeHeader (SPSettings_ opts) = case opts.encodeHeader of SPSettingsEncodeHeader_ f -> f

reportRequestError :: forall a. AjaxRequest -> (a -> ErrorDescription) -> a -> AjaxError
reportRequestError req' err msg = makeAjaxError req' $ reportError err msg

reportError :: forall a err. (a -> err) -> a -> err
reportError err msg = err msg

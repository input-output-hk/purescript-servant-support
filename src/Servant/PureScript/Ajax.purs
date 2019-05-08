{--
  This file contains code copied from the purescript-affjax project from slamdata.
  It is therefore licensed under Apache License version 2.0.
--}

module Servant.PureScript.Ajax where

import Prelude

import Affjax (Request, Response, request, printResponseFormatError)
import Affjax as Affjax
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.Error.Class (class MonadError, catchError, throwError)
import Control.Monad.Except (runExcept, mapExcept)
import Data.Argonaut.Core (Json)
import Data.Array ((..), zipWith, length)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.List.NonEmpty (toList)
import Data.MediaType.Common (applicationJSON)
import Data.Traversable (sequence)
import Effect.Aff (Aff, message)
import Effect.Aff.Class (class MonadAff, liftAff)
import Foreign (renderForeignError, MultipleErrors, F, readInt, readArray, Foreign, ForeignError(..))
import Foreign.Generic (genericDecode)
import Foreign.Generic.Class (class GenericDecode)
import Foreign.Generic.Types (Options)
import Foreign.JSON (decodeJSONWith)
import Servant.PureScript.JsUtils (unsafeToString)
import Servant.PureScript.Settings (SPSettings_(..), SPSettingsDecodeJson_(..))


newtype AjaxError
  = AjaxError
    { request     :: Request Unit
    , description :: ErrorDescription
    }

data ErrorDescription
  = DecodingError String
  | ConnectionError String
  | ResponseFormatError String


makeAjaxError :: Request Unit -> ErrorDescription -> AjaxError
makeAjaxError req desc = 
  AjaxError 
    { request : req
    , description : desc
    }

runAjaxError :: AjaxError -> { request :: Request Unit, description :: ErrorDescription }
runAjaxError (AjaxError err) = err

errorToString :: AjaxError -> String
errorToString = unsafeToString

requestToString :: Request Json -> String
requestToString = unsafeToString

responseToString :: forall res. Response res -> String
responseToString = unsafeToString

class FromJSON a where
  fromJSON :: Options -> Foreign -> F a

instance intFromJSON :: FromJSON Int where
  fromJSON _ = readInt

else instance unitFromJSON :: FromJSON Unit where
  fromJSON _ _ = pure unit

else instance arrayFromJSON :: FromJSON a => FromJSON (Array a) where
  fromJSON opts = readArray >=> readElements where
    readElements :: Array Foreign -> F (Array a)
    readElements arr = sequence (zipWith readElement (0 .. length arr) arr)

    readElement :: Int -> Foreign -> F a
    readElement i value = mapExcept (lmap (map (ErrorAtIndex i))) (fromJSON opts value)

else instance genericFromJSON :: (Generic a rep, GenericDecode rep) => FromJSON a where
  fromJSON = genericDecode

-- | Do an affjax call but report Aff exceptions in our own MonadError
ajax :: forall m res params. FromJSON res => MonadError AjaxError m => MonadAff m
        => SPSettings_ params -> Request Unit -> m (Response res)
ajax (SPSettings_ settings) req = do
  let headers = [ContentType applicationJSON] <> req.headers
  response <- liftWithError $ request (req { responseFormat = ResponseFormat.string, headers = headers })
  responseBody <- toFormatError response.body
  decoded <- toDecodingError <<< runExcept $ decodeJSONWith (fromJSON decodeOptions) responseBody
  pure $ response { body = decoded }
  where
    decodeOptions :: Options
    decodeOptions = let (SPSettingsDecodeJson_ options) = settings.decodeJson in options

    liftWithError :: forall a. Aff a -> m a
    liftWithError action = do
      res <- liftAff $ toEither action
      toAjaxError res

    toEither :: forall a. Aff a -> Aff (Either String a)
    toEither action = catchError (Right <$> action) $ \e ->
      pure $ Left (message e)

    toAjaxError :: forall a. Either String a -> m a
    toAjaxError r = case r of
        Left err -> throwError $ makeAjaxError req $ ConnectionError err
        Right v  -> pure v

    toFormatError :: forall a. Either Affjax.ResponseFormatError a -> m a
    toFormatError r = case r of
        Left err -> throwError $ makeAjaxError req $ ResponseFormatError (printResponseFormatError err)
        Right v  -> pure v

    toDecodingError :: forall a. Either MultipleErrors a -> m a
    toDecodingError r = case r of
        Left err -> throwError $ makeAjaxError req $ DecodingError (show (toList (map renderForeignError err)))
        Right v  -> pure v

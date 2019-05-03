{--
  This file contains code copied from the purescript-affjax project from slamdata.
  It is therefore licensed under Apache License version 2.0.
--}

module Servant.PureScript.Ajax where

import Prelude

import Data.List.NonEmpty (toList)
import Data.Argonaut.Core (Json)
import Control.Monad.Error.Class (class MonadError, catchError, throwError)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Effect.Aff (Aff, message)
import Effect.Aff.Class (class MonadAff, liftAff)
import Foreign.Generic (genericDecodeJSON, defaultOptions)
import Foreign.Generic.Class (class GenericDecode)
import Foreign (renderForeignError, MultipleErrors)
import Affjax (Request, Response, request, printResponseFormatError)
import Affjax as Affjax
import Affjax.ResponseFormat as ResponseFormat
import Servant.PureScript.JsUtils (unsafeToString)
import Control.Monad.Except (runExcept)


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


-- | Do an affjax call but report Aff exceptions in our own MonadError
ajax :: forall m res rep. Generic res rep => GenericDecode rep => MonadError AjaxError m => MonadAff m
        => Request Unit -> m (Response res)
ajax req = do
  response <- liftWithError $ request (req { responseFormat = ResponseFormat.string })
  responseBody <- toFormatError response.body
  decoded <- toDecodingError <<< runExcept $ genericDecodeJSON defaultOptions responseBody
  pure $ response { body = decoded }
  where
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

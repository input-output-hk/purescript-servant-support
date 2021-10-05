{--
  This file contains code copied from the purescript-affjax project from slamdata.
  It is therefore licensed under Apache License version 2.0.
--}

module Servant.PureScript.Affjax where

import Prelude
import Affjax (Response)
import Affjax.ResponseHeader (ResponseHeader(..))
import Control.Monad.Error.Class (throwError, catchError, class MonadError)
import Data.Argonaut.Decode (JsonDecodeError)
import Data.Either (Either(Left, Right))
import Data.Function.Uncurried (Fn5, runFn5, Fn4, runFn4)
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.Nullable (Nullable, toNullable)
import Effect (Effect)
import Effect.Aff (Aff, Canceler(Canceler), makeAff)
import Effect.Aff.Class (liftAff, class MonadAff)
import Effect.Exception (message, Error)
import Web.XHR.XMLHttpRequest (XMLHttpRequest)

newtype AjaxError = AjaxError
  { request :: AjaxRequest
  , description :: ErrorDescription
  }

data ErrorDescription = UnexpectedHTTPStatus (Response String)
                      | ParsingError String
                      | DecodingError JsonDecodeError
                      | ConnectionError String

type AjaxRequest =
  { method :: String
  , url :: String
  , headers :: Array { field :: String, value :: String }
  , content :: Nullable String
  , responseType :: String
  , username :: Nullable String
  , password :: Nullable String
  , withCredentials :: Boolean
  }

makeAjaxError :: AjaxRequest -> ErrorDescription -> AjaxError
makeAjaxError req desc = AjaxError { request : req
                                   , description : desc
                                   }

runAjaxError :: AjaxError -> { request :: AjaxRequest, description :: ErrorDescription}
runAjaxError (AjaxError err) = err

errorToString :: AjaxError -> String
errorToString = unsafeToString

requestToString :: AjaxRequest -> String
requestToString = unsafeToString

responseToString :: Response String -> String
responseToString = unsafeToString

defaultRequest :: AjaxRequest
defaultRequest = {
    method : "GET"
  , url : ""
  , headers : [ {field : "Accept", value : "application/json"}
              , {field : "content-type", value : "application/json"}]
  , content : toNullable (Nothing :: Maybe String)
  , responseType : "text"
  , username : toNullable (Nothing :: Maybe String)
  , password : toNullable (Nothing :: Maybe String)
  , withCredentials : false
  }


-- | Do an affjax call but report Aff exceptions in our own MonadError
affjax
  :: forall m
   . MonadError AjaxError m
  => MonadAff m
  => AjaxRequest
  -> m (Response String)
affjax req = toAjaxError <=< liftAff <<< toEither $ makeAff \cb -> ajax req (cb <<< Left) (cb <<< Right)
  where
    toEither :: forall a. Aff a -> Aff (Either String a)
    toEither action = catchError (Right <$> action) $ \e ->
     pure $ Left (message e)

    toAjaxError :: forall a. Either String a -> m a
    toAjaxError r = case r of
        Left err -> throwError $ AjaxError
                                  { request : req
                                  , description : ConnectionError err
                                  }
        Right v  -> pure v

ajax :: forall e.
     AjaxRequest
  -> (Error -> Effect  Unit)
  -> (Response String -> Effect Unit)
  -> Effect Canceler
ajax req eb cb = runFn5 _ajax ResponseHeader req cancelAjax eb cb


foreign import _ajax
  :: forall e. Fn5 (String -> String -> ResponseHeader)
               AjaxRequest
               (XMLHttpRequest String -> Canceler)
               (Error -> Effect Unit)
               (Response String -> Effect Unit)
               (Effect Canceler)

cancelAjax :: forall a e. XMLHttpRequest a -> Canceler
cancelAjax xhr = Canceler \err -> makeAff (\cb -> mempty <$ runFn4 _cancelAjax xhr err (cb <<< Left) (const <<< cb $ Right unit))

foreign import _cancelAjax
  :: forall e a. Fn4 (XMLHttpRequest a)
                   Error
                   (Error -> Effect Unit)
                   (Boolean -> Effect Unit)
                   (Effect Unit)

foreign import unsafeToString :: forall obj. obj -> String

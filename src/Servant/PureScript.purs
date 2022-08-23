module Servant.PureScript where

import Prelude

import Affjax (Error)
import Affjax.Web as Affjax
import Affjax.RequestBody (RequestBody(..))
import Affjax.RequestBody as RequestBody
import Affjax.RequestHeader (RequestHeader)
import Affjax.ResponseFormat (ResponseFormat)
import Affjax.ResponseFormat as Response
import Control.Monad.Cont (ContT)
import Control.Monad.Except (ExceptT)
import Control.Monad.Identity.Trans (IdentityT)
import Control.Monad.Maybe.Trans (MaybeT)
import Control.Monad.RWS (RWST)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (WriterT)
import Data.Argonaut
  ( Json
  , JsonDecodeError
  , caseJsonString
  , printJsonDecodeError
  , stringify
  , stringifyWithIndent
  )
import Data.Array (fromFoldable)
import Data.Array.NonEmpty as NEA
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), hush)
import Data.HTTP.Method (CustomMethod, Method)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.NonEmpty ((:|))
import Data.String (Pattern(..), joinWith, split)
import Data.String.NonEmpty as NES
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Type.Proxy (Proxy(..))
import URI (Fragment, Host, Path, PathAbsolute(..), RelativeRef, UserInfo)
import URI.Extra.QueryPairs
  ( Key
  , QueryPairs(..)
  , Value
  , keyFromString
  , valueFromString
  )
import URI.Extra.QueryPairs as QueryPairs
import URI.Host as Host
import URI.Path.Segment (segmentFromString, segmentNZFromString)
import URI.RelativeRef as RR
import Web.DOM (Document)
import Web.File.Blob (Blob)

class ToPathSegment a where
  toPathSegment :: a -> String

instance ToPathSegment String where
  toPathSegment = identity

instance ToPathSegment Json where
  toPathSegment value = caseJsonString (stringify value) identity value

class ToQueryValue a where
  toQueryValue :: a -> Value

instance ToQueryValue String where
  toQueryValue = valueFromString

instance ToQueryValue Json where
  toQueryValue value =
    valueFromString $ caseJsonString (stringify value) identity value

class ToHeader a where
  toHeader :: a -> String

instance ToHeader String where
  toHeader = identity

instance ToHeader Json where
  toHeader value = caseJsonString (stringify value) identity value

flagQueryPairs :: String -> Boolean -> QueryPairs Key Value
flagQueryPairs name true = QueryPairs [ Tuple (keyFromString name) Nothing ]
flagQueryPairs _ _ = QueryPairs []

paramQueryPairs
  :: forall a. ToQueryValue a => String -> Maybe a -> QueryPairs Key Value
paramQueryPairs name = paramListQueryPairs name <<< fromFoldable

paramListQueryPairs
  :: forall a. ToQueryValue a => String -> Array a -> QueryPairs Key Value
paramListQueryPairs name =
  QueryPairs <<< map (Tuple (keyFromString name) <<< Just <<< toQueryValue)

type RequestUri =
  RelativeRef UserInfo Host Path (Array String) (QueryPairs Key Value) Fragment

type Request reqContent resContent decodeError req res =
  { method :: Either Method CustomMethod
  , uri :: RequestUri
  , headers :: Array RequestHeader
  , content :: Maybe req
  , encode :: req -> reqContent
  , decode :: resContent -> Either decodeError res
  }

class Eq content <= ContentType error content | content -> error where
  responseFormat :: Proxy content -> ResponseFormat content
  requestBody :: content -> RequestBody
  fromRequestBody :: RequestBody -> Maybe content
  serializeContent :: content -> String
  serializeError :: Proxy content -> error -> String
  caseArrayView :: forall r. (ArrayBuffer -> r) -> content -> Maybe r
  caseBlob :: forall r. (Blob -> r) -> content -> Maybe r
  caseDocument :: forall r. (Document -> r) -> content -> Maybe r
  caseJson :: forall r. (Json -> r) -> content -> Maybe r
  caseString :: forall r. (String -> r) -> content -> Maybe r
  caseIgnore :: forall r. (Unit -> r) -> content -> Maybe r

instance ContentType Void String where
  responseFormat _ = Response.string
  requestBody = RequestBody.string
  fromRequestBody = case _ of
    RequestBody.String s -> Just s
    _ -> Nothing
  serializeContent = identity
  serializeError _ = absurd
  caseArrayView = const $ const Nothing
  caseBlob = const $ const Nothing
  caseDocument = const $ const Nothing
  caseJson = const $ const Nothing
  caseString = map Just
  caseIgnore = const $ const Nothing

instance ContentType JsonDecodeError Json where
  responseFormat _ = Response.json
  requestBody = RequestBody.json
  fromRequestBody = case _ of
    RequestBody.Json j -> Just j
    _ -> Nothing
  serializeContent = stringifyWithIndent 2
  serializeError _ = printJsonDecodeError
  caseArrayView = const $ const Nothing
  caseBlob = const $ const Nothing
  caseDocument = const $ const Nothing
  caseJson = map Just
  caseString = const $ const Nothing
  caseIgnore = const $ const Nothing

-- | Monads that can perform ajax requests. As stock instance for Aff calls
-- | Affjax.request without modifying the request. Custom instances can be used
-- | to extend ajax requests with arbitrary functionality. Here is a
-- | non-exhaustive list of possibile enhancements:
-- |
-- |   - Replace the path in the request with one that points to an API server
-- |     configured in the app's environment.
-- |   - Log request failures including JSON decoding errors.
-- |   - Handle authentication and token persistence transparently.
-- |   - Add and handle common headers to requests.
-- |
class Monad m <= MonadAjax api m where
  request
    :: forall reqDecodeError resDecodeError resContent reqContent req res
     . ContentType reqDecodeError reqContent
    => ContentType resDecodeError resContent
    => api
    -> Request reqContent resContent resDecodeError req res
    -> m (Either (AjaxError resDecodeError resContent) res)

instance MonadAjax api Aff where
  request _ req =
    prepareResponse req.decode aReq <$> Affjax.request aReq
    where
    aReq = prepareRequest req

prepareResponse
  :: forall decodeError content res
   . ContentType decodeError content
  => (content -> Either decodeError res)
  -> Affjax.Request content
  -> (Either Affjax.Error (Affjax.Response content))
  -> Either (AjaxError decodeError content) res
prepareResponse decode req mResponse = lmap mkError do
  response <- lmap ConnectingError mResponse
  let status = unwrap response.status
  if status < 200 || status >= 300 then
    Left UnexpectedHTTPStatus
  else
    lmap MalformedContent $ decode response.body
  where
  mkError description = AjaxError
    { request: req
    , response: hush mResponse
    , description
    }

prepareRequest
  :: forall reqDecodeError resDecodeError resContent reqContent req res
   . ContentType reqDecodeError reqContent
  => ContentType resDecodeError resContent
  => Request reqContent resContent resDecodeError req res
  -> Affjax.Request resContent
prepareRequest req = Affjax.defaultRequest
  { method = req.method
  , url = RR.print
      { printUserInfo: identity
      , printHosts: Host.print
      , printPath: identity
      , printRelPath: Left <<< segmentsToPathAbsolute
      , printQuery: QueryPairs.print identity identity
      , printFragment: identity
      }
      req.uri
  , headers = req.headers
  , content = requestBody <<< req.encode <$> req.content
  , responseFormat = responseFormat (Proxy :: _ resContent)
  }

segmentsToPathAbsolute :: Array String -> PathAbsolute
segmentsToPathAbsolute =
  PathAbsolute <<< (convertNonEmpty <<< NEA.toNonEmpty <=< NEA.fromArray)
  where
  convertNonEmpty (segmentNZ :| segments) = Tuple
    <$> (segmentNZFromString <$> NES.fromString segmentNZ)
    <*> pure (segmentFromString <$> segments)

instance MonadAjax api m => MonadAjax api (ContT r m) where
  request api = lift <<< request api

instance MonadAjax api m => MonadAjax api (IdentityT m) where
  request api = lift <<< request api

instance MonadAjax api m => MonadAjax api (ExceptT e' m) where
  request api = lift <<< request api

instance MonadAjax api m => MonadAjax api (MaybeT m) where
  request api = lift <<< request api

instance (Monoid w, MonadAjax api m) => MonadAjax api (RWST r w s m) where
  request api = lift <<< request api

instance MonadAjax api m => MonadAjax api (ReaderT r m) where
  request api = lift <<< request api

instance (Monoid w, MonadAjax api m) => MonadAjax api (WriterT w m) where
  request api = lift <<< request api

instance MonadAjax api m => MonadAjax api (StateT s m) where
  request api = lift <<< request api

newtype AjaxError decodeError content =
  AjaxError
    { request :: Affjax.Request content
    , response :: Maybe (Affjax.Response content)
    , description :: ErrorDescription decodeError
    }

data ErrorDescription decodeError
  = UnexpectedHTTPStatus
  | MalformedContent decodeError
  | ConnectingError Error

derive instance Functor ErrorDescription

printAjaxError
  :: forall decodeError content
   . ContentType decodeError content
  => AjaxError decodeError content
  -> String
printAjaxError (AjaxError { request, response, description }) =
  joinWith "\n" $ join
    [ [ "Error making web request:" ]
    , [ "Request Info:" ]
    , (" " <> _) <$>
        [ "URI: " <> request.url
        , "Method: " <> show request.method
        , "Headers: " <> show request.headers
        , append "Body: " $ show $ flip map request.content $ case _ of
            ArrayView _ -> "ArrayView"
            Blob _ -> "Blob"
            Document _ -> "Document"
            String s -> s
            FormData _ -> "FormData"
            FormURLEncoded fue -> show fue
            Json json -> stringifyWithIndent 2 json
        ]
    , fromFoldable response >>= \resp -> join
        [ [ "Response Info:" ]
        , (" " <> _) <$>
            [ "StatusCode: " <> show resp.status
            , "StatusText: " <> show resp.statusText
            , "Headers: " <> show resp.headers
            , "Body: " <> serializeContent resp.body
            ]
        ]
    , [ "Failure:" ]
    , ("  " <> _) <$> split (Pattern "\n") case description of
        UnexpectedHTTPStatus -> "Unexpected HTTP Status"
        MalformedContent error -> serializeError (Proxy :: _ content) error
        ConnectingError error -> Affjax.printError error
    ]

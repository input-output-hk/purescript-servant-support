module Servant.PureScript where

import Prelude

import Affjax (Error)
import Affjax as Affjax
import Affjax.RequestBody (RequestBody(..))
import Affjax.RequestHeader (RequestHeader)
import Affjax.ResponseFormat (ResponseFormat)
import Control.Apply (lift2)
import Control.Monad.Cont (ContT)
import Control.Monad.Except (ExceptT)
import Control.Monad.Maybe.Trans (MaybeT)
import Control.Monad.RWS (RWST)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (WriterT)
import Data.Argonaut (stringifyWithIndent)
import Data.Array (fromFoldable)
import Data.Bifunctor (class Bifunctor, bimap, lmap)
import Data.Either (Either(..))
import Data.HTTP.Method (CustomMethod, Method)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.String (Pattern(..), joinWith, split)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import URI (RelativeRef)
import URI.RelativeRef (RelativeRefPrintOptions)
import URI.RelativeRef as RR

newtype ResponseT content m e a =
  ResponseT
    ( m
        { request :: Affjax.Request content
        , response :: Maybe (Affjax.Response content)
        , result :: Either (ErrorDescription e) a
        }
    )

runResponse
  :: forall content m e a
   . ResponseT content m e a
  -> m
       { request :: Affjax.Request content
       , response :: Maybe (Affjax.Response content)
       , result :: Either (ErrorDescription e) a
       }
runResponse (ResponseT r) = r

derive instance Functor m => Functor (ResponseT content m e)

instance Functor m => Bifunctor (ResponseT content m) where
  bimap f g (ResponseT m) =
    ResponseT $ m <#> \r -> r { result = bimap (map f) g r.result }

instance Apply m => Apply (ResponseT content m e) where
  apply (ResponseT r1) (ResponseT r2) = ResponseT $ lift2 apply' r1 r2
    where
    apply' r@{ result: Left e } _ =
      r { result = Left e }
    apply' _ r@{ result: Left e } =
      r { result = Left e }
    apply' { result: Right f } r@{ result: Right a } =
      r { result = Right (f a) }

instance Monad m => Bind (ResponseT content m e) where
  bind (ResponseT m) f = ResponseT $ bind m f'
    where
    f' r@{ result: Left e } = pure r { result = Left e }
    f' { result: Right a } = case f a of
      ResponseT m' -> m'

hoistResponseT
  :: forall content m n e
   . m ~> n
  -> ResponseT content m e ~> ResponseT content n e
hoistResponseT phi (ResponseT m) = ResponseT $ phi m

type Request userInfo hosts path relPath query content e req res =
  { method :: Either Method CustomMethod
  , uri :: RelativeRef userInfo hosts path relPath query Void
  , uriPrintOptions ::
      { | RelativeRefPrintOptions userInfo hosts path relPath query Void () }
  , headers :: Array RequestHeader
  , content :: Maybe req
  , encode :: req -> RequestBody
  , decode :: content -> Either e res
  , responseFormat :: ResponseFormat content
  }

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
class MonadAff m <= MonadAjax m where
  request
    :: forall userInfo hosts path relPath query content e req res
     . Request userInfo hosts path relPath query content e req res
    -> ResponseT content m e res

instance MonadAjax Aff where
  request req = do
    response <- mkResponse affjaxReq Nothing ConnectingError $ Affjax.request
      affjaxReq
    let status = unwrap response.status
    if status < 200 || status >= 300 then
      mkResponse affjaxReq (Just response) (const UnexpectedHTTPStatus)
        $ pure
        $ Left unit
    else
      mkResponse affjaxReq (Just response) DecodingError $ pure $ req.decode
        response.body
    where
    affjaxReq = Affjax.defaultRequest
      { method = req.method
      , url = RR.print req.uriPrintOptions req.uri
      , headers = req.headers
      , content = req.encode <$> req.content
      , responseFormat = req.responseFormat
      }

    mkResponse
      :: forall a content e' e
       . Affjax.Request content
      -> Maybe (Affjax.Response content)
      -> (e' -> ErrorDescription e)
      -> Aff (Either e' a)
      -> ResponseT content Aff e a
    mkResponse r response mkError m = ResponseT $ m <#> \result ->
      { request: r
      , response
      , result: lmap mkError result
      }

instance MonadAjax m => MonadAjax (ContT r m) where
  request = hoistResponseT lift <<< request

instance MonadAjax m => MonadAjax (ExceptT e m) where
  request = hoistResponseT lift <<< request

instance MonadAjax m => MonadAjax (MaybeT m) where
  request = hoistResponseT lift <<< request

instance (Monoid w, MonadAjax m) => MonadAjax (RWST r w s m) where
  request = hoistResponseT lift <<< request

instance MonadAjax m => MonadAjax (ReaderT r m) where
  request = hoistResponseT lift <<< request

instance (Monoid w, MonadAjax m) => MonadAjax (WriterT w m) where
  request = hoistResponseT lift <<< request

instance MonadAjax m => MonadAjax (StateT s m) where
  request = hoistResponseT lift <<< request

data ErrorDescription decodeError
  = UnexpectedHTTPStatus
  | DecodingError decodeError
  | ConnectingError Error

derive instance Functor ErrorDescription

runResponseT
  :: forall content m e e' a
   . Functor m
  => ( Affjax.Request content
       -> Maybe (Affjax.Response content)
       -> ErrorDescription e
       -> e'
     )
  -> ResponseT content m e a
  -> m (Either e' a)
runResponseT handleError (ResponseT m) = m <#> case _ of
  { result: Right a } -> Right a
  r@{ result: Left e } -> Left $ handleError r.request r.response e

printError
  :: forall content m e
   . Functor m
  => (content -> String)
  -> (e -> String)
  -> Affjax.Request content
  -> Maybe (Affjax.Response content)
  -> ErrorDescription e
  -> String
printError printResponseBody printDecodeError request response description =
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
            , "Body: " <> printResponseBody resp.body
            ]
        ]
    , [ "Failure:" ]
    , ("  " <> _) <$> split (Pattern "\n") case description of
        UnexpectedHTTPStatus -> "Unexpected HTTP Status"
        DecodingError error -> printDecodeError error
        ConnectingError error -> Affjax.printError error
    ]

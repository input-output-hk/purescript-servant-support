module Servant.PureScript where

import Prelude

import Affjax (Error, Request, Response, printError)
import Affjax as Affjax
import Control.Monad.Cont (ContT)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (ExceptT(..), except, mapExceptT, withExceptT)
import Control.Monad.Maybe.Trans (MaybeT)
import Control.Monad.RWS (RWST)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Monad.Writer (WriterT)
import Data.Argonaut
  ( Json
  , JsonDecodeError
  , printJsonDecodeError
  , stringify
  , stringifyWithIndent
  )
import Data.Array ((:))
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Bifunctor (lmap)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Either (Either)
import Data.Foldable (class Foldable, foldMap)
import Data.Identity (Identity)
import Data.Int (decimal)
import Data.Int as Int
import Data.List (List)
import Data.List.Lazy as LL
import Data.List.Lazy.NonEmpty as NLL
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Data.NonEmpty (NonEmpty)
import Data.Number.Format as Number
import Data.String (Pattern(..), drop, joinWith, split)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.UUID (UUID)
import Data.UUID as UUID
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Type.Proxy (Proxy)

foreign import encodeURIComponent :: String -> String

-- | Class for types that can be encoded as a peice of a URI.
-- | The resulting string should URL encoded.
class ToURLPiece a where
  toURLPiece :: a -> String

instance ToURLPiece String where
  toURLPiece = encodeURIComponent

instance ToURLPiece Boolean where
  toURLPiece = encodeURIComponent <<< show

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

instance ToURLPiece a => ToURLPiece (Array a) where
  toURLPiece = toURLPieceFoldable

instance ToURLPiece a => ToURLPiece (Maybe a) where
  toURLPiece = toURLPieceFoldable

instance ToURLPiece a => ToURLPiece (Identity a) where
  toURLPiece = toURLPieceFoldable

instance ToURLPiece a => ToURLPiece (Either e a) where
  toURLPiece = toURLPieceFoldable

instance ToURLPiece a => ToURLPiece (List a) where
  toURLPiece = toURLPieceFoldable

instance ToURLPiece a => ToURLPiece (LL.List a) where
  toURLPiece = toURLPieceFoldable

instance (Foldable f, ToURLPiece a) => ToURLPiece (NonEmpty f a) where
  toURLPiece = toURLPieceFoldable

instance ToURLPiece a => ToURLPiece (NonEmptyArray a) where
  toURLPiece = toURLPieceFoldable

instance ToURLPiece a => ToURLPiece (NonEmptyList a) where
  toURLPiece = toURLPieceFoldable

instance ToURLPiece a => ToURLPiece (NLL.NonEmptyList a) where
  toURLPiece = toURLPieceFoldable

instance IsSymbol sym => ToURLPiece (Proxy sym) where
  toURLPiece = toURLPiece <<< reflectSymbol

-- | An implementation of toURLPiece that uses a foldable instance
toURLPieceFoldable :: forall f a. Foldable f => ToURLPiece a => f a -> String
toURLPieceFoldable = drop 1 <<< foldMap \a -> toURLPiece a <> "/"

type AjaxError =
  { request :: Request Json
  , description :: ErrorDescription
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
    :: forall a
     . (Json -> Either JsonDecodeError a)
    -> Request Json
    -> ExceptT AjaxError m a

instance MonadAjax Aff where
  request decode req = do
    response <-
      withExceptT (mkError <<< ConnectingError) $ ExceptT $ Affjax.request req
    let status = unwrap response.status
    when (status < 200 || status >= 300) do
      throwError $ mkError $ UnexpectedHTTPStatus response
    except $ lmap (mkError <<< DecodingError) $ decode response.body
    where
    mkError description = { request: req, description }

instance MonadAjax m => MonadAjax (ContT r m) where
  request = liftRequest

instance MonadAjax m => MonadAjax (ExceptT e m) where
  request = liftRequest

instance MonadAjax m => MonadAjax (MaybeT m) where
  request = liftRequest

instance (Monoid w, MonadAjax m) => MonadAjax (RWST r w s m) where
  request = liftRequest

instance MonadAjax m => MonadAjax (ReaderT r m) where
  request = liftRequest

instance (Monoid w, MonadAjax m) => MonadAjax (WriterT w m) where
  request = liftRequest

instance MonadAjax m => MonadAjax (StateT s m) where
  request = liftRequest

-- | A version of request that works with any monad transformer.
liftRequest
  :: forall t m a
   . MonadAjax m
  => MonadTrans t
  => (Json -> Either JsonDecodeError a)
  -> Request Json
  -> ExceptT AjaxError (t m) a
liftRequest = map (map (mapExceptT lift)) request

data ErrorDescription
  = UnexpectedHTTPStatus (Response Json)
  | DecodingError JsonDecodeError
  | ConnectingError Error

-- | Pretty-print an AjaxError
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

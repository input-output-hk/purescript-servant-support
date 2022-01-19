module Servant.PureScript where

import Prelude

import Affjax (Error, Request, Response, printError)
import Affjax as Affjax
import Control.Monad.Cont (ContT)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (ExceptT(..), except, mapExceptT, withExceptT)
import Control.Monad.Identity.Trans (IdentityT)
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
import Data.Bifunctor (lmap)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Either (Either)
import Data.Foldable (class Foldable, foldMap)
import Data.Int (decimal)
import Data.Int as Int
import Data.Newtype (unwrap)
import Data.Number.Format as Number
import Data.String (Pattern(..), drop, joinWith, split)
import Data.UUID (UUID)
import Data.UUID as UUID
import Effect.Aff (Aff)

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

instance ToURLPiece a => ToURLPiece (Array a) where
  toURLPiece = toURLPieceFoldable

toURLPieceFoldable :: forall f a. Foldable f => ToURLPiece a => f a -> String
toURLPieceFoldable = drop 1 <<< foldMap \a -> toURLPiece a <> "/"

type AjaxError =
  { request :: Request Json
  , description :: ErrorDescription
  }

class Monad m <= MonadAjax m where
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

instance MonadAjax m => MonadAjax (IdentityT m) where
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

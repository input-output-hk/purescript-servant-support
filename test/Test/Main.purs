module Test.Main where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (List(..))
import Data.List.NonEmpty (NonEmptyList(..))
import Data.NonEmpty ((:|))
import Effect (Effect)
import Foreign (ForeignError(..), MultipleErrors)
import Foreign.Generic.Types (Options)
import Foreign.JSON (decodeJSONWith)
import Global.Unsafe (unsafeStringify)
import Servant.PureScript.Ajax (class FromJSON, class ToJSON, fromJSON, toJSON)
import Servant.PureScript.Settings (SPSettingsDecodeJson_(..), SPSettingsEncodeJson_(..), SPSettings_(..), defaultSettings)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest do
  suite "JSON Handling" do
    suite "Int" do
      2 `roundTripFrom` "2"

    suite "String" do
      "Woo" `roundTripFrom` "\"Woo\""

    suite "Unit" do
      unit `roundTripFrom` "{}"

    suite "Either" do
      (Left 5       :: Either Int String) `roundTripFrom` "{\"Left\":5}"
      (Right "Test" :: Either Int String) `roundTripFrom` "{\"Right\":\"Test\"}"

    suite "Array" do
      [1,2,3,4] `roundTripFrom` "[1,2,3,4]"
      test "Errors" do
        equal
          (Left (NonEmptyList ((ErrorAtIndex 1 (TypeMismatch "Int" "String")) :| Nil)))
          (decode "[1,\"x\",3,\"y\"]" :: Either MultipleErrors (Array Int))

    suite "Generic" do
      (User { name: "Tester" }) `roundTripFrom` "{\"contents\":{\"name\":\"Tester\"},\"tag\":\"User\"}"

------------------------------------------------------------

newtype User = User
  { name :: String }

derive instance genericUser :: Generic User _
derive instance eqUser :: Eq User

instance showUser :: Show User where
  show = genericShow

------------------------------------------------------------

roundTripFrom :: forall a. Show a => Eq a => ToJSON a => FromJSON a => a -> String -> TestSuite
roundTripFrom value str =
  suite (show value) do
    test "Decoding" do
      equal (Right value) (decode str)
    test "Encoding" do
      equal str (encode value)
    test "Roundtrip" do
      equal (Right value) (decode (encode value))

decode :: forall a. FromJSON a => String -> Either MultipleErrors a
decode str =
  runExcept $ decodeJSONWith (fromJSON decodeOptions) str

  where
    (SPSettings_ settings) = defaultSettings unit

    decodeOptions :: Options
    decodeOptions = let (SPSettingsDecodeJson_ options) = settings.decodeJson in options

encode :: forall a. ToJSON a => a -> String
encode value =
  unsafeStringify $ toJSON encodeOptions value

  where
    (SPSettings_ settings) = defaultSettings unit

    encodeOptions :: Options
    encodeOptions = let (SPSettingsEncodeJson_ options) = settings.encodeJson in options

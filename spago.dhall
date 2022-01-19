{ name = "servant-support"
, dependencies =
  [ "aff"
  , "affjax"
  , "argonaut"
  , "arrays"
  , "bifunctors"
  , "either"
  , "newtype"
  , "prelude"
  , "psci-support"
  , "strings"
  , "transformers"
  , "uri"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}

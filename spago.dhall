{ name = "servant-support"
, dependencies =
  [ "aff"
  , "affjax"
  , "argonaut"
  , "arrays"
  , "bifunctors"
  , "either"
  , "http-methods"
  , "maybe"
  , "newtype"
  , "nonempty"
  , "prelude"
  , "psci-support"
  , "strings"
  , "transformers"
  , "tuples"
  , "uri"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}

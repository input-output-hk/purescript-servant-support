{ name = "servant-support"
, dependencies =
  [ "affjax"
  , "argonaut"
  , "arrays"
  , "bigints"
  , "integers"
  , "newtype"
  , "numbers"
  , "prelude"
  , "psci-support"
  , "strings"
  , "uuid"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}

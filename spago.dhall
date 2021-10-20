{ name = "servant-support"
, dependencies =
  [ "affjax"
  , "argonaut-codecs"
  , "argonaut-core"
  , "prelude"
  , "psci-support"

  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}

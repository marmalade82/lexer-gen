{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "catenable-lists"
  , "console"
  , "effect"
  , "foreign-generic"
  , "psci-support"
  , "spec"
  , "spec-discovery"
  , "strings"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}

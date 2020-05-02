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
  , "node-child-process"
  , "node-process"
  , "profunctor-lenses"
  , "psci-support"
  , "spec"
  , "spec-discovery"
  , "strings"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}

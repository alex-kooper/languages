{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
, name =
    "my-project"
, dependencies =
    [ "console"
    , "effect"
    , "filterable"
    , "integers"
    , "node-fs"
    , "ordered-collections"
    , "psci-support"
    , "stringutils"
    , "tuples"
    , "unicode"
    ]
, packages =
    ./packages.dhall
}

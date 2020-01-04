{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies = 
    [ "console"
    , "effect"
    , "halogen"
    , "halogen-css"
    , "psci-support" 
    , "random" 
    , "ordered-collections"
    , "assert"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}

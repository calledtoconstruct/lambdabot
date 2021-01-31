import Test.DocTest
import Data.List

packages = [
  "lambdabot-core-5.1.0.4"
  , "mtl"
  , "time"
  ]

main =
  doctest $
    [ "-isrc"
    , "Lambdabot.Plugin.Term"
    , "Lambdabot.Plugin.Term.Term"
    , "Lambdabot.Plugin.Term.Logic"
    , "Lambdabot.Plugin.Term.Configuration"
    , "Lambdabot.Config.Term"
    ] ++ concat (zipWith (\x y -> [x, y]) (replicate (length packages) "-package" ) packages)
    
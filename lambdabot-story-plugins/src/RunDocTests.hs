import Test.DocTest
import Data.List

packages = [
  "lambdabot-core-5.1.0.4"
    , "mtl"
    , "lifted-base"
  ]

main =
  doctest $
    [ "-isrc"
    , "Lambdabot.Plugin.Story"
    , "Lambdabot.Plugin.Story.Story"
    , "Lambdabot.Plugin.Story.Configuration"
    , "Lambdabot.Config.Story"
    ] ++ concat (zipWith (\x y -> [x, y]) (replicate (length packages) "-package" ) packages)
    
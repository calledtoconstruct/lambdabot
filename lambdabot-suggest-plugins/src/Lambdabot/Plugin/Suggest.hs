module Lambdabot.Plugin.Suggest (
  suggestPlugin,
  suggestPlugins,
) where

import Lambdabot.Plugin.Suggest.Suggest (suggestPlugin)

suggestPlugins :: [String]
suggestPlugins = ["suggest"]

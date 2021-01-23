module Lambdabot.Plugin.Suggest (
  suggestPlugin,
  module Lambdabot.Config.Suggest,
) where

import Lambdabot.Config.Suggest ()
import Lambdabot.Plugin.Suggest.Suggest (suggestPlugin)

suggestPlugins :: [String]
suggestPlugins = ["suggest", "suggestions", "remove-suggestion"]

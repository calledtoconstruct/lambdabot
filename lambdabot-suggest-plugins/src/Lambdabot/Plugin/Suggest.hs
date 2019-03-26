
module Lambdabot.Plugin.Suggest (
  suggestPlugin,
  module Lambdabot.Config.Suggest
) where

import Lambdabot.Config.Suggest
import Lambdabot.Plugin.Suggest.Suggest

suggestPlugins :: [String]
suggestPlugins = ["suggest", "suggestions", "remove-suggestion"]

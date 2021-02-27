module Lambdabot.Plugin.Term (
  module Lambdabot.Config.Term,
  termPlugin,
  termPlugins
) where

import Lambdabot.Config.Term (defaultTerms)
import Lambdabot.Plugin.Term.Term (termPlugin)

termPlugins :: [String]
termPlugins = ["term"]
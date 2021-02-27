module Lambdabot.Plugin.Story (
  module Lambdabot.Config.Story,
  storyPlugin,
  storyPlugins
) where

import Lambdabot.Config.Story (defaultStories)
import Lambdabot.Plugin.Story.Story (storyPlugin)

storyPlugins :: [String]
storyPlugins = ["story"]
module Lambdabot.Plugin.Social (
  activityPlugin,
  karmaPlugin,
  pollPlugin,
  seenPlugin,
  tellPlugin,
  socialPlugins,
) where

import Lambdabot.Plugin.Social.Activity (activityPlugin)
import Lambdabot.Plugin.Social.Karma (karmaPlugin)
import Lambdabot.Plugin.Social.Poll (pollPlugin)
import Lambdabot.Plugin.Social.Seen (seenPlugin)
import Lambdabot.Plugin.Social.Tell (tellPlugin)

socialPlugins :: [String]
socialPlugins = ["activity", "karma", "poll", "seen", "tell"]

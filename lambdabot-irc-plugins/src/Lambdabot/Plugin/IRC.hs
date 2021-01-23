module Lambdabot.Plugin.IRC (
  ircPlugin,
  localtimePlugin,
  logPlugin,
  topicPlugin,
  ircPlugins,
) where

import Lambdabot.Plugin.IRC.IRC (ircPlugin)
import Lambdabot.Plugin.IRC.Localtime (localtimePlugin)
import Lambdabot.Plugin.IRC.Log (logPlugin)
import Lambdabot.Plugin.IRC.Topic (topicPlugin)

ircPlugins :: [String]
ircPlugins = ["irc", "localtime", "log", "topic"]

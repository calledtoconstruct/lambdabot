module Lambdabot.Plugin.Misc (
  dummyPlugin,
  errorPlugin,
  freshPlugin,
  helloPlugin,
  statsPlugin,
  todoPlugin,
  miscPlugins,
) where

import Lambdabot.Plugin.Misc.Dummy (dummyPlugin)
import Lambdabot.Plugin.Misc.Error (errorPlugin)
import Lambdabot.Plugin.Misc.Fresh (freshPlugin)
import Lambdabot.Plugin.Misc.Hello (helloPlugin)
import Lambdabot.Plugin.Misc.Stats (statsPlugin)
import Lambdabot.Plugin.Misc.Todo (todoPlugin)

miscPlugins :: [String]
miscPlugins = ["dummy", "error", "fresh", "hello", "stats", "todo"]

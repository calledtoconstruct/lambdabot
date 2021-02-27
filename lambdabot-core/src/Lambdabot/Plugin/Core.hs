module Lambdabot.Plugin.Core (
  basePlugin,
  systemPlugin,
  offlineRCPlugin,
  composePlugin,
  helpPlugin,
  morePlugin,
  versionPlugin,
  corePlugins,
) where

import Lambdabot.Plugin.Core.Base (basePlugin)
import Lambdabot.Plugin.Core.Compose (composePlugin)
import Lambdabot.Plugin.Core.Help (helpPlugin)
import Lambdabot.Plugin.Core.More (morePlugin)
import Lambdabot.Plugin.Core.OfflineRC (offlineRCPlugin)
import Lambdabot.Plugin.Core.System (systemPlugin)
import Lambdabot.Plugin.Core.Version (versionPlugin)

corePlugins :: [String]
corePlugins =
  ["base", "system", "offlineRC", "compose", "help", "more", "version"]

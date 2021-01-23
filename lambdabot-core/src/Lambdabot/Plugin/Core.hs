module Lambdabot.Plugin.Core (
  basePlugin,
  systemPlugin,
  offlineRCPlugin,
  composePlugin,
  helpPlugin,
  morePlugin,
  versionPlugin,
  corePlugins,
  module Lambdabot.Config.Core,
) where

import Lambdabot.Config.Core (
  commandPrefixes,
  consoleLogFormat,
  consoleLogHandle,
  consoleLogLevel,
  dataDir,
  disabledCommands,
  editDistanceLimit,
  enableInsults,
  lbRootLoggerPath,
  lbVersion,
  onShutdownCmds,
  onStartupCmds,
  outputDir,
  replaceRootLogger,
  textWidth,
  uncaughtExceptionHandler,
 )
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

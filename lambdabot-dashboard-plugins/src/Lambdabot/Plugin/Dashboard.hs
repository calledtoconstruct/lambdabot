module Lambdabot.Plugin.Dashboard (
  module Lambdabot.Config.Dashboard,
  dashboardPlugin,
  dashboardPlugins
) where

import Lambdabot.Config.Dashboard (dashboardPort)
import Lambdabot.Plugin.Dashboard.Dashboard (dashboardPlugin)

dashboardPlugins :: [String]
dashboardPlugins = ["dashboard"]
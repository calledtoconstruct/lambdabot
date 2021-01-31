module Lambdabot.Plugin.Points (
  pointsPlugin,
  pointsPlugins,
) where

import Lambdabot.Config.Points ()
import Lambdabot.Plugin.Points.Points (pointsPlugin)

pointsPlugins :: [String]
pointsPlugins = ["points"]

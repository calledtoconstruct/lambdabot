
module Lambdabot.Plugin.Points (
  pointsPlugin,
  module Lambdabot.Config.Points
) where

import Lambdabot.Config.Points
import Lambdabot.Plugin.Points.Points

pointsPlugins :: [String]
pointsPlugins = ["points", "leaderboard", "give-points"]

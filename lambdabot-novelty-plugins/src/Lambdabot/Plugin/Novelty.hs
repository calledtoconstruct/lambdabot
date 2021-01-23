module Lambdabot.Plugin.Novelty (
  bfPlugin,
  dicePlugin,
  elitePlugin,
  filterPlugin,
  numberwangPlugin,
  quotePlugin,
  slapPlugin,
  unlambdaPlugin,
  vixenPlugin,
  noveltyPlugins,
  module Lambdabot.Config.Novelty,
) where

import Lambdabot.Config.Novelty (bfBinary, unlambdaBinary)
import Lambdabot.Plugin.Novelty.BF (bfPlugin)
import Lambdabot.Plugin.Novelty.Dice (dicePlugin)
import Lambdabot.Plugin.Novelty.Elite (elitePlugin)
import Lambdabot.Plugin.Novelty.Filter (filterPlugin)
import Lambdabot.Plugin.Novelty.Numberwang (numberwangPlugin)
import Lambdabot.Plugin.Novelty.Quote (quotePlugin)
import Lambdabot.Plugin.Novelty.Slap (slapPlugin)
import Lambdabot.Plugin.Novelty.Unlambda (unlambdaPlugin)
import Lambdabot.Plugin.Novelty.Vixen (vixenPlugin)

noveltyPlugins :: [String]
noveltyPlugins = ["bf", "dice", "elite", "filter", "numberwang", "quote", "slap", "unlambda", "vixen"]

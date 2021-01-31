{-# LANGUAGE TemplateHaskell #-}

module Modules (modulesInfo) where

import Lambdabot.Main (Modules, basePlugin, composePlugin, helpPlugin, modules, morePlugin, offlineRCPlugin, systemPlugin, versionPlugin)

-- to add a new plugin, one must first add a qualified import here, and also
-- add a string in the list below

import Lambdabot.Plugin.Hangman (hangmanPlugin, hangmanPlugins)
import Lambdabot.Plugin.Haskell (checkPlugin, evalPlugin, freePlugin, haddockPlugin, haskellPlugins, hooglePlugin, instancesPlugin, plPlugin, pointfulPlugin, prettyPlugin, sourcePlugin, typePlugin, undoPlugin, unmtlPlugin)
import Lambdabot.Plugin.IRC (ircPlugin, ircPlugins, localtimePlugin, logPlugin, topicPlugin)
import Lambdabot.Plugin.Misc (dummyPlugin, errorPlugin, freshPlugin, helloPlugin, miscPlugins, todoPlugin)
import Lambdabot.Plugin.Novelty (dicePlugin, elitePlugin, filterPlugin, noveltyPlugins, numberwangPlugin, quotePlugin, slapPlugin, unlambdaPlugin, vixenPlugin)
import Lambdabot.Plugin.Points (pointsPlugin, pointsPlugins)
import Lambdabot.Plugin.Reference (dictPlugin, metarPlugin, oeisPlugin, referencePlugins, searchPlugin, spellPlugin, tickerPlugin, urlPlugin, wherePlugin)
import Lambdabot.Plugin.Social (activityPlugin, karmaPlugin, pollPlugin, seenPlugin, socialPlugins, tellPlugin)
import Lambdabot.Plugin.Suggest (suggestPlugin, suggestPlugins)
import Lambdabot.Plugin.Term (termPlugin, termPlugins)
import Lambdabot.Plugin.Twitch (twitchPlugin, twitchPlugins)

import Lambdabot.Plugin.Core (corePlugins)

modulesInfo :: Modules
modulesInfo =
  $( modules $
      corePlugins
        ++ hangmanPlugins
        ++ haskellPlugins
        ++ ircPlugins
        ++ miscPlugins
        ++ noveltyPlugins
        ++ pointsPlugins
        ++ referencePlugins
        ++ socialPlugins
        ++ suggestPlugins
        ++ twitchPlugins
        ++ termPlugins
   )
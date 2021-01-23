-- {-# LANGUAGE TemplateHaskell #-}

module Modules (modulesInfo) where

import Lambdabot.Main (Modules, basePlugin, composePlugin, helpPlugin, morePlugin, offlineRCPlugin, systemPlugin, versionPlugin)

-- to add a new plugin, one must first add a qualified import here, and also
-- add a string in the list below
-- import Lambdabot.Plugin.Haskell

import Lambdabot.Plugin.Hangman (hangmanPlugin)
import Lambdabot.Plugin.IRC (ircPlugin, localtimePlugin, topicPlugin)
import Lambdabot.Plugin.Misc (dummyPlugin, freshPlugin, todoPlugin)
import Lambdabot.Plugin.Novelty (bfPlugin, dicePlugin, elitePlugin, filterPlugin, numberwangPlugin, quotePlugin, slapPlugin, unlambdaPlugin, vixenPlugin)
import Lambdabot.Plugin.Points (pointsPlugin)
import Lambdabot.Plugin.Reference (dictPlugin, metarPlugin, oeisPlugin, searchPlugin, spellPlugin, tickerPlugin, urlPlugin, wherePlugin)
import Lambdabot.Plugin.Social (activityPlugin, karmaPlugin, pollPlugin, seenPlugin, tellPlugin)
import Lambdabot.Plugin.Suggest (suggestPlugin)
import Lambdabot.Plugin.Twitch (twitchPlugin)

import Data.Some (Some (Some))
import Lambdabot.Plugin.Haskell (typePlugin)

modulesInfo :: Modules
modulesInfo =
  [ ("twitch", Some twitchPlugin)
  , ("points", Some pointsPlugin)
  , ("hangman", Some hangmanPlugin)
  , ("suggest", Some suggestPlugin)
  , ("irc", Some ircPlugin)
  , ("localtime", Some localtimePlugin)
  , ("topic", Some topicPlugin)
  , ("dummy", Some dummyPlugin)
  , ("fresh", Some freshPlugin)
  , ("todo", Some todoPlugin)
  , ("bf", Some bfPlugin)
  , ("dice", Some dicePlugin)
  , ("elite", Some elitePlugin)
  , ("filter", Some filterPlugin)
  , ("quote", Some quotePlugin)
  , ("slap", Some slapPlugin)
  , ("unlambda", Some unlambdaPlugin)
  , ("dict", Some dictPlugin)
  , ("metar", Some metarPlugin)
  , ("oeis", Some oeisPlugin)
  , ("search", Some searchPlugin)
  , ("spell", Some spellPlugin)
  , ("ticker", Some tickerPlugin)
  , ("url", Some urlPlugin)
  , ("where", Some wherePlugin)
  , ("activity", Some activityPlugin)
  , ("karma", Some karmaPlugin)
  , ("poll", Some pollPlugin)
  , ("seen", Some seenPlugin)
  , ("tell", Some tellPlugin)
  , ("base", Some basePlugin)
  , ("system", Some systemPlugin)
  , ("offlineRC", Some offlineRCPlugin)
  , ("compose", Some composePlugin)
  , ("help", Some helpPlugin)
  , ("more", Some morePlugin)
  , ("version", Some versionPlugin)
  , ("numberwang", Some numberwangPlugin)
  , ("vixen", Some vixenPlugin)
  , ("type", Some typePlugin)
  ]

-- {-# LANGUAGE TemplateHaskell #-}

module Modules
  ( modulesInfo
  )
where

import           Lambdabot.Main

-- to add a new plugin, one must first add a qualified import here, and also
-- add a string in the list below
-- import Lambdabot.Plugin.Haskell
import           Lambdabot.Plugin.IRC
import           Lambdabot.Plugin.Twitch
import           Lambdabot.Plugin.Points
import           Lambdabot.Plugin.Suggest
import           Lambdabot.Plugin.Misc
import           Lambdabot.Plugin.Novelty
import           Lambdabot.Plugin.Reference
import           Lambdabot.Plugin.Social
import           Data.Some

modulesInfo :: Modules
modulesInfo =
  [ ("twitch"   , This twitchPlugin)
  , ("points"   , This pointsPlugin)
  , ("suggest"  , This suggestPlugin)
  , ("irc"      , This ircPlugin)
  , ("localtime", This localtimePlugin)
  , ("topic"    , This topicPlugin)
  , ("dummy"    , This dummyPlugin)
  , ("fresh"    , This freshPlugin)
  , ("todo"     , This todoPlugin)
  , ("bf"       , This bfPlugin)
  , ("dice"     , This dicePlugin)
  , ("elite"    , This elitePlugin)
  , ("filter"   , This filterPlugin)
  , ("quote"    , This quotePlugin)
  , ("slap"     , This slapPlugin)
  , ("unlambda" , This unlambdaPlugin)
  , ("dict"     , This dictPlugin)
  , ("metar"    , This metarPlugin)
  , ("oeis"     , This oeisPlugin)
  , ("search"   , This searchPlugin)
  , ("spell"    , This spellPlugin)
  , ("ticker"   , This tickerPlugin)
  , ("url"      , This urlPlugin)
  , ("where"    , This wherePlugin)
  , ("activity" , This activityPlugin)
  , ("karma"    , This karmaPlugin)
  , ("poll"     , This pollPlugin)
  , ("seen"     , This seenPlugin)
  , ("tell"     , This tellPlugin)
  , ("base"     , This basePlugin)
  , ("system"   , This systemPlugin)
  , ("offlineRC", This offlineRCPlugin)
  , ("compose"  , This composePlugin)
  , ("help"     , This helpPlugin)
  , ("more"     , This morePlugin)
  , ("version"  , This versionPlugin)
  ]

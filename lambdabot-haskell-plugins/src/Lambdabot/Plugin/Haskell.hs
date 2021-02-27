module Lambdabot.Plugin.Haskell (
  checkPlugin,
  djinnPlugin,
  evalPlugin,
  freePlugin,
  haddockPlugin,
  hooglePlugin,
  instancesPlugin,
  plPlugin,
  pointfulPlugin,
  prettyPlugin,
  sourcePlugin,
  typePlugin,
  undoPlugin,
  unmtlPlugin,
  haskellPlugins,
  trustedPackages,
  languageExts,
) where

import Lambdabot.Config.Haskell (languageExts, trustedPackages)
import Lambdabot.Plugin.Haskell.Check (checkPlugin)
import Lambdabot.Plugin.Haskell.Djinn (djinnPlugin)
import Lambdabot.Plugin.Haskell.Eval (evalPlugin)
import Lambdabot.Plugin.Haskell.Free (freePlugin)
import Lambdabot.Plugin.Haskell.Haddock (haddockPlugin)
import Lambdabot.Plugin.Haskell.Hoogle (hooglePlugin)
import Lambdabot.Plugin.Haskell.Instances (instancesPlugin)
import Lambdabot.Plugin.Haskell.Pl (plPlugin)
import Lambdabot.Plugin.Haskell.Pointful (pointfulPlugin)
import Lambdabot.Plugin.Haskell.Pretty (prettyPlugin)
import Lambdabot.Plugin.Haskell.Source (sourcePlugin)
import Lambdabot.Plugin.Haskell.Type (typePlugin)
import Lambdabot.Plugin.Haskell.UnMtl (unmtlPlugin)
import Lambdabot.Plugin.Haskell.Undo (undoPlugin)

haskellPlugins :: [String]
haskellPlugins =
  [ "check"
  , -- , "djinn"
    "eval"
  , "free"
  , "haddock"
  , "hoogle"
  , "instances"
  , "pl"
  , "pointful"
  , "pretty"
  , "source"
  , "type"
  , "undo"
  , "unmtl"
  ]

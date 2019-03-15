
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns -Wmissing-signatures #-}

module Lambdabot.Config.Twitch (
  reconnectDelay,
  onStartupCmds
) where

import Lambdabot.Config

config "reconnectDelay" [t| Int |] [| 10000000 |]
configWithMerge [| (++) |] "onStartupCmds" [t| [String] |] [| ["rc scripts/twitch.rc"]   |]

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Lambdabot.Config.Dashboard (
  dashboardPort,
  maximumNumberOfMessages,
  garbageCollectionIntervalInSeconds,
) where

import Lambdabot.Config (config)

config "dashboardPort" [t|Int|] [|8080|]
config "maximumNumberOfMessages" [t|Int|] [|500|]
config "garbageCollectionIntervalInSeconds" [t|Int|] [|5 * 60|]

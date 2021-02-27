{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Lambdabot.Config.Dashboard (
  dashboardPort,
  maximumNumberOfMessages,
  maximumNumberOfMessagesPerChannel,
  garbageCollectionIntervalInSeconds,
) where

import Lambdabot.Config (config)

maxNumMsgPerChannel :: Int
maxNumMsgPerChannel = 500

maxNumMsg :: Int
maxNumMsg = maxNumMsgPerChannel * 30

config "dashboardPort" [t|Int|] [|8080|]
config "maximumNumberOfMessagesPerChannel" [t|Int|] [|maxNumMsgPerChannel|]
config "maximumNumberOfMessages" [t|Int|] [|maxNumMsg|]
config "garbageCollectionIntervalInSeconds" [t|Int|] [|5 * 60|]

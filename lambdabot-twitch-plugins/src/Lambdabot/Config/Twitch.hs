{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns -Wmissing-signatures #-}

module Lambdabot.Config.Twitch (
  reconnectDelay,
) where

import Lambdabot.Config (config)

config "reconnectDelay" [t|Int|] [|10000000|]

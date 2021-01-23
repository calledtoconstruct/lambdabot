{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Lambdabot.Config.Points (
  pointsPerMessage,
  pointsPerMinute,
) where

import Lambdabot.Config (config)

config "pointsPerMinute" [t|Int|] [|1|]
config "pointsPerMessage" [t|Int|] [|10|]

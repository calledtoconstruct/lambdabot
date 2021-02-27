{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

module Lambdabot.Config.Term (
  termFrequency,
  defaultTerms,
  secondsToWaitBeforeUnlockingTerm,
) where

import Lambdabot.Config (config)

import Data.Int (Int64)

config "termFrequency" [t|(Int, Int)|] [|(3, 5)|]
config "secondsToWaitBeforeUnlockingTerm" [t|Int64|] [|5 * 60|]
config
  "defaultTerms"
  [t|[([String], String)]|]
  [|
    [ (["CPU", "cpu"], "Central Processing Unit, a general puprose computing device used in modern computers.")
    , (["Haskell", "haskell"], "The Haskell programming language.  A (mostly) pure function programming language.")
    , (["GHC", "ghc"], "The glorious Glasgow Haskell Compiler.")
    , (["lambdabot"], "The glorious chat bot written in Haskell!")
    , (["swarm"], "A large or dense group of software engineers who tackle a problem or task together.")
    ]
    |]

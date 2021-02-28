{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

module Lambdabot.Config.Term (
  termFrequency,
  defaultTerms,
  secondsToWaitBeforeUnlockingTerm,
) where

import Lambdabot.Config (config)

import Data.Int (Int64)
import qualified Data.Text as T

config "termFrequency" [t|(Int, Int)|] [|(3, 5)|]
config "secondsToWaitBeforeUnlockingTerm" [t|Int64|] [|5 * 60|]
config
  "defaultTerms"
  [t|[([T.Text], T.Text)]|]
  [|
    [ ([T.pack "CPU", T.pack "cpu"], T.pack "Central Processing Unit, a general puprose computing device used in modern computers.")
    , ([T.pack "Haskell", T.pack "haskell"], T.pack "The Haskell programming language.  A (mostly) pure function programming language.")
    , ([T.pack "GHC", T.pack "ghc"], T.pack "The glorious Glasgow Haskell Compiler.")
    , ([T.pack "lambdabot"], T.pack "The glorious chat bot written in Haskell!")
    , ([T.pack "swarm"], T.pack "A large or dense group of software engineers who tackle a problem or task together.")
    ]
    |]

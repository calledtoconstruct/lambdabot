{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Lambdabot.Config.Hangman (
  hangmanPhrases,
) where

import Lambdabot.Config (config)

config
  "hangmanPhrases"
  [t|[String]|]
  [|
    [ "MONKATOS"
    , "TWITCH SINGS"
    , "BEST STREAMER"
    , "IN REAL LIFE"
    , "SCIENCE AND TECHNOLOGY"
    , "SOFTWARE ENGINEERING"
    , "HASKELL RULEZ"
    ]
    |]

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Lambdabot.Config.Hangman (
  hangmanPhrases,
) where

import Lambdabot.Config (config)
import qualified Data.Text as T

config
  "hangmanPhrases"
  [t|[T.Text]|]
  [|
    [ "MONKATOS"
    , "BEST STREAMER"
    , "IN REAL LIFE"
    , "SCIENCE AND TECHNOLOGY"
    , "SOFTWARE ENGINEERING"
    , "HASKELL RULEZ"
    ]
    |]

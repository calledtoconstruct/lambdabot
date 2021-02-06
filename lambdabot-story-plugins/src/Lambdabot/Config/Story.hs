{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

module Lambdabot.Config.Story (
  secondsToWaitBeforeMovingToTheNextWord,
  defaultStories
) where

import Lambdabot.Config (config)

config "secondsToWaitBeforeMovingToTheNextWord" [t|Int|] [|25|]
config
  "defaultStories"
  [t|[(String, String)]|]
  [|
    [ ("Friend", "I have a friend.  My friend is a __noun:animal__ .  They are kind of __adjective__ and __adjective__ at the zoo.  But they are my buddy, my buddy to stay, until they __verb__ up and __verb__ away."),
      ("Knocking", "__noun:person__ , __noun:person__ knocking at my door.  I want to __verb__ __noun:place__ but I don't know if I can.  I'm so afraid of __noun:person__ .")
    ]
    |]

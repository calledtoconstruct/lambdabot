
module Lambdabot.Plugin.Hangman (
  hangmanPlugin,
  module Lambdabot.Config.Hangman
) where

import Lambdabot.Config.Hangman
import Lambdabot.Plugin.Hangman.Hangman

hangmanPlugins :: [String]
hangmanPlugins = ["hangman-start", "hangman-status", "hm"]

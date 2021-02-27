module Lambdabot.Plugin.Hangman (
  module Lambdabot.Config.Hangman,
  hangmanPlugin,
  hangmanPlugins
) where

import Lambdabot.Config.Hangman (hangmanPhrases)
import Lambdabot.Plugin.Hangman.Hangman (hangmanPlugin)

hangmanPlugins :: [String]
hangmanPlugins = ["hangman"]
module Lambdabot.Plugin.Hangman.Manage where

import Lambdabot.Plugin.Hangman.Configuration
import Lambdabot.Plugin.Hangman.Game

import Data.Char (toUpper)
import Data.Universe.Helpers ((+++))
import Data.List.Split (splitOn)

addPhrase :: Game -> String -> Result
addPhrase previous phrase
  | null invalidCharacters = Result [messagePhraseAdded configuration] $ addPhraseToGame previous upperPhrase
  | otherwise = Result ["The phrase contains one or more invalid characters, therefore, it was not added."] previous
  where configuration = getConfiguration previous
        upperPhrase = map toUpper phrase
        invalidCharacters = filter (`notElem` validCharacters) upperPhrase

addPhraseToGame :: Game -> String -> Game
addPhraseToGame (NoGame configuration) phrase = NoGame $ addPhraseToConfiguration configuration phrase
addPhraseToGame (InGame gameState configuration) phrase = InGame gameState $ addPhraseToConfiguration configuration phrase

addPhraseToConfiguration :: Configuration -> String -> Configuration
addPhraseToConfiguration configuration phrase = configuration { phrases = upperPhrase: (phrases configuration) }
  where upperPhrase = map toUpper phrase

removePhrase :: Game -> String -> Result
removePhrase previous phrase = Result [messagePhraseRemoved configuration] $ removePhraseFromGame previous phrase
  where configuration = getConfiguration previous

removePhraseFromGame :: Game -> String -> Game
removePhraseFromGame (NoGame configuration) phrase = NoGame $ removePhraseFromConfiguration configuration phrase
removePhraseFromGame (InGame gameState configuration) phrase = InGame gameState $ removePhraseFromConfiguration configuration phrase

removePhraseFromConfiguration :: Configuration -> String -> Configuration
removePhraseFromConfiguration configuration phrase = configuration { phrases = filtered }
  where filtered = filter (/= upperPhrase) $ phrases configuration
        upperPhrase = map toUpper phrase

configure :: Game -> String -> Result
configure previous input = configureGame previous parameters
  where parameters = splitOn " " input

configureGame :: Game -> [String] -> Result
configureGame (NoGame configuration) parameters = Result output $ NoGame updatedConfiguration
  where (output, updatedConfiguration) = configureConfiguration configuration parameters
configureGame (InGame gameState configuration) parameters = Result output $ InGame gameState updatedConfiguration
  where (output, updatedConfiguration) = configureConfiguration configuration parameters

configureConfiguration :: Configuration -> [String] -> ([String], Configuration)
configureConfiguration configuration [] = ([messageIncorrectArgumentsForConfigure], configuration)
configureConfiguration configuration [_] = ([messageIncorrectArgumentsForConfigure], configuration)
configureConfiguration configuration (option: rest) = case result of
  Just updatedConfiguration -> ([messageConfigurationUpdated], updatedConfiguration)
  Nothing -> ([messageUnknownConfigurationOption], configuration)
  where result = updateConfiguration configuration option value
        value = concat $ (+++) rest $ replicate numberOfSegments " "
        numberOfSegments = flip (-) 1 $ length rest

updateConfiguration :: Configuration -> String -> String -> Maybe Configuration
updateConfiguration configuration "messageYouWon" value = Just configuration { messageYouWon = value }
updateConfiguration configuration "messageYouLost" value = Just configuration { messageYouLost = value }
updateConfiguration configuration "messageThereWereNoGuesses" value = Just configuration { messageThereWereNoGuesses = value }
updateConfiguration configuration "messageNewGameHasBegun" value = Just configuration { messageNewGameHasBegun = value }
updateConfiguration configuration "messageIncorrectGuessesTried" value = Just configuration { messageIncorrectGuessesTried = value }
updateConfiguration configuration "messageNumberOfGuessesRemaining" value = Just configuration { messageNumberOfGuessesRemaining = value }
updateConfiguration configuration "messageGuessing" value = Just configuration { messageGuessing = value }
updateConfiguration configuration "allowedMisses" value = Just configuration { allowedMisses = read value }
updateConfiguration _ _ _ = Nothing
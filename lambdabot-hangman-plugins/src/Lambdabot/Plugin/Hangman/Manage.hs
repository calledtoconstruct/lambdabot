{-# LANGUAGE OverloadedStrings #-}

module Lambdabot.Plugin.Hangman.Manage where

import Lambdabot.Plugin.Hangman.Configuration (
  Configuration (
    initialAllowedMisses,
    messageAlreadyGuessed,
    messageCorrect,
    messageGuessing,
    messageIncorrect,
    messageIncorrectGuessesTried,
    messageNewGameHasBegun,
    messageNumberOfGuessesRemaining,
    messageOutcome,
    messagePhraseAdded,
    messagePhraseRemoved,
    messageThereWereNoGuesses,
    messageYouLost,
    messageYouWon,
    phrases
  ),
  Message,
  Phrase,
  validCharacters, elemText, upperText
 )
import Lambdabot.Plugin.Hangman.Game (
  Game (..),
  Result (Result),
  getConfiguration,
  messageConfigurationUpdated,
  messageIncorrectArgumentsForConfigure,
  messageUnknownConfigurationOption,
 )

import qualified Data.Text as T
import qualified Data.Text.Read as T

addPhrase :: Game -> Phrase -> Result
addPhrase previous phrase
  | isValid = Result [messagePhraseAdded configuration] $ addPhraseToGame previous (upperText phrase)
  | otherwise = Result ["The phrase contains one or more invalid characters, therefore, it was not added."] previous
 where
  configuration = getConfiguration previous
  isValid = T.all (`elemText` (validCharacters `T.append` " ")) (upperText phrase)

addPhraseToGame :: Game -> Phrase -> Game
addPhraseToGame (NoGame configuration) phrase = NoGame $ addPhraseToConfiguration configuration phrase
addPhraseToGame (InGame gameState configuration) phrase = InGame gameState $ addPhraseToConfiguration configuration phrase

addPhraseToConfiguration :: Configuration -> Phrase -> Configuration
addPhraseToConfiguration configuration phrase = configuration{phrases = upperText phrase : phrases configuration}

removePhrase :: Game -> Phrase -> Result
removePhrase previous phrase = Result [messagePhraseRemoved configuration] $ removePhraseFromGame previous phrase
 where
  configuration = getConfiguration previous

removePhraseFromGame :: Game -> Phrase -> Game
removePhraseFromGame (NoGame configuration) phrase = NoGame $ removePhraseFromConfiguration configuration phrase
removePhraseFromGame (InGame gameState configuration) phrase = InGame gameState $ removePhraseFromConfiguration configuration phrase

removePhraseFromConfiguration :: Configuration -> Phrase -> Configuration
removePhraseFromConfiguration configuration phrase = configuration{phrases = filter (/= upperText phrase) (phrases configuration)}

configure :: Game -> Message -> Result
configure previous input = configureGame previous (T.words input)

configureGame :: Game -> [Message] -> Result
configureGame (NoGame configuration) parameters = Result output $ NoGame updatedConfiguration
 where
  (output, updatedConfiguration) = configureConfiguration configuration parameters
configureGame (InGame gameState configuration) parameters = Result output $ InGame gameState updatedConfiguration
 where
  (output, updatedConfiguration) = configureConfiguration configuration parameters

configureConfiguration :: Configuration -> [Message] -> ([Message], Configuration)
configureConfiguration configuration [] = ([messageIncorrectArgumentsForConfigure], configuration)
configureConfiguration configuration [_] = ([messageIncorrectArgumentsForConfigure], configuration)
configureConfiguration configuration (option : rest) = case updateConfiguration configuration option (T.unwords rest) of
  Just updatedConfiguration -> ([messageConfigurationUpdated], updatedConfiguration)
  Nothing -> ([messageUnknownConfigurationOption], configuration)

updateConfiguration :: Configuration -> Message -> Message -> Maybe Configuration
updateConfiguration configuration "messageYouWon" value = Just configuration{messageYouWon = value}
updateConfiguration configuration "messageYouLost" value = Just configuration{messageYouLost = value}
updateConfiguration configuration "messageThereWereNoGuesses" value = Just configuration{messageThereWereNoGuesses = value}
updateConfiguration configuration "messageNewGameHasBegun" value = Just configuration{messageNewGameHasBegun = value}
updateConfiguration configuration "messageIncorrectGuessesTried" value = Just configuration{messageIncorrectGuessesTried = value}
updateConfiguration configuration "messageNumberOfGuessesRemaining" value = Just configuration{messageNumberOfGuessesRemaining = value}
updateConfiguration configuration "messageGuessing" value = Just configuration{messageGuessing = value}
updateConfiguration configuration "messageCorrect" value = Just configuration{messageCorrect = value}
updateConfiguration configuration "messageIncorrect" value = Just configuration{messageIncorrect = value}
updateConfiguration configuration "messageAlreadyGuessed" value = Just configuration{messageAlreadyGuessed = value}
updateConfiguration configuration "messageOutcome" value = Just configuration{messageOutcome = value}
updateConfiguration configuration "initialAllowedMisses" value = case T.decimal value of
  Right (n, _) -> Just configuration{initialAllowedMisses = n}
  Left _ -> Nothing
updateConfiguration _ _ _ = Nothing

{-# LANGUAGE DeriveGeneric #-}

module Lambdabot.Plugin.Hangman.Configuration (
  Configuration (Configuration),
  phrases,
  lastPhrase,
  messagePhraseAdded,
  messagePhraseRemoved,
  messageYouWon,
  messageYouLost,
  messageThereWereNoGuesses,
  messageNewGameHasBegun,
  messageIncorrectGuessesTried,
  messageNumberOfGuessesRemaining,
  messageGuessing,
  messageCorrect,
  messageIncorrect,
  messageAlreadyGuessed,
  messageOutcome,
  allowedMisses,
  newConfiguration,
  selectPhrase,
  validCharacters,
) where

import GHC.Generics (Generic)
import System.Random (mkStdGen, random)

data Configuration = Configuration
  { phrases :: [String]
  , lastPhrase :: Int
  , allowedMisses :: Int
  , messagePhraseAdded :: String
  , messagePhraseRemoved :: String
  , messageYouWon :: String
  , messageYouLost :: String
  , messageThereWereNoGuesses :: String
  , messageNewGameHasBegun :: String
  , messageIncorrectGuessesTried :: String
  , messageNumberOfGuessesRemaining :: String
  , messageGuessing :: String
  , messageCorrect :: String
  , messageIncorrect :: String
  , messageAlreadyGuessed :: String
  , messageOutcome :: String
  }
  deriving (Generic, Show, Read)

newConfiguration :: [String] -> Configuration
newConfiguration allThePhrases =
  Configuration
    { phrases = allThePhrases
    , lastPhrase = 0
    , allowedMisses = 10
    , messagePhraseAdded = "Phrase added"
    , messagePhraseRemoved = "Phrase removed"
    , messageYouWon = "You win!"
    , messageYouLost = "You lost!"
    , messageThereWereNoGuesses = "There were no guesses!  Use ?hangman-guess [letter] to add a letter you believe is in the phrase.  The most popular guess will be evaluated."
    , messageNewGameHasBegun = "A new game of Hangman has begun!  Guess the first letter using:  ?hangman-guess [letter]"
    , messageIncorrectGuessesTried = "The following guesses were incorrect or duplicate: [@]"
    , messageNumberOfGuessesRemaining = "You will lose if you make @ more mistake(s)."
    , messageGuessing = "You are guessing this phrase: [@]"
    , messageCorrect = "correct"
    , messageIncorrect = "incorrect"
    , messageAlreadyGuessed = "already guessed"
    , messageOutcome = "The popular guess was @ and that was @."
    }

selectPhrase :: Configuration -> (Configuration, String)
selectPhrase configuration = (updatedConfiguration, phrase)
 where
  lengthOfPhrases = length listOfPhrases
  generator = mkStdGen $ lastPhrase configuration
  selected = fst $ random generator
  phrase = listOfPhrases !! (selected `mod` lengthOfPhrases)
  updatedConfiguration = configuration{lastPhrase = selected}
  listOfPhrases = phrases configuration

validLetters :: String
validLetters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

validNumbers :: String
validNumbers = "0123456789"

validSymbols :: String
validSymbols = "`~!@#$%^&*()-_=+[{]}\\|;:\'\",<.>/?"

validCharacters :: String
validCharacters = validLetters ++ validNumbers ++ validSymbols

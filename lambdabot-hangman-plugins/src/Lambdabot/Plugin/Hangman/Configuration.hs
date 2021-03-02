{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Lambdabot.Plugin.Hangman.Configuration (
  Configuration (Configuration),
  Phrase,
  Message,
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
  messageGuessed,
  messageCorrect,
  messageIncorrect,
  messageAlreadyGuessed,
  messageOutcome,
  initialAllowedMisses,
  defaultSecondsBetweenCycles,
  defaultWarningAt,
  newConfiguration,
  selectPhrase,
  validCharacters,
  elemText,
  upperText,
) where

import GHC.Generics (Generic)
import System.Random (mkStdGen, random)
import qualified Data.Text as T
import Data.Char (toUpper)

type Phrase = T.Text
type Message = T.Text

data Configuration = Configuration
  { phrases :: [Phrase]
  , lastPhrase :: Int
  , initialAllowedMisses :: Int
  , defaultSecondsBetweenCycles :: Int
  , defaultWarningAt :: [Int]
  , messagePhraseAdded :: Message
  , messagePhraseRemoved :: Message
  , messageYouWon :: Message
  , messageYouLost :: Message
  , messageThereWereNoGuesses :: Message
  , messageNewGameHasBegun :: Message
  , messageIncorrectGuessesTried :: Message
  , messageNumberOfGuessesRemaining :: Message
  , messageGuessing :: Message
  , messageGuessed :: Message
  , messageCorrect :: Message
  , messageIncorrect :: Message
  , messageAlreadyGuessed :: Message
  , messageOutcome :: Message
  }
  deriving (Generic, Show, Read)

newConfiguration :: [Phrase] -> Configuration
newConfiguration allThePhrases =
  Configuration
    { phrases = allThePhrases
    , lastPhrase = 0
    , initialAllowedMisses = 10
    , defaultSecondsBetweenCycles = 30
    , defaultWarningAt = [10, 3, 2, 1]
    , messagePhraseAdded = "Phrase added"
    , messagePhraseRemoved = "Phrase removed"
    , messageYouWon = "You win!"
    , messageYouLost = "You lost!"
    , messageThereWereNoGuesses = "There were no guesses!  Use ?hangman-guess [letter] to add a letter you believe is in the phrase.  The most popular guess will be evaluated."
    , messageNewGameHasBegun = "A new game of Hangman has begun!  Guess the first letter using:  ?hangman-guess [letter]"
    , messageIncorrectGuessesTried = "The following guesses were incorrect or duplicate: [@]"
    , messageNumberOfGuessesRemaining = "You will lose if you make @ more mistake(s)."
    , messageGuessing = "You are guessing this phrase: [@]"
    , messageGuessed = "You guessed the phrase: [@]"
    , messageCorrect = "correct"
    , messageIncorrect = "incorrect"
    , messageAlreadyGuessed = "already guessed"
    , messageOutcome = "The popular guess was @ and that was @."
    }

selectPhrase :: Configuration -> (Configuration, Phrase)
selectPhrase configuration = (updatedConfiguration, phrase)
 where
  lengthOfPhrases = length listOfPhrases
  generator = mkStdGen $ lastPhrase configuration
  selected = fst $ random generator
  phrase = listOfPhrases !! (selected `mod` lengthOfPhrases)
  updatedConfiguration = configuration{lastPhrase = selected}
  listOfPhrases = phrases configuration

validLetters :: Phrase
validLetters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

validNumbers :: Phrase
validNumbers = "0123456789"

validSymbols :: Phrase
validSymbols = "`~!@#$%^&*()-_=+[{]}\\|;:\'\",<.>/?"

validCharacters :: Phrase
validCharacters = validLetters `T.append` validNumbers `T.append` validSymbols

elemText :: Char -> T.Text -> Bool
elemText c = ((c ==) `T.any`)

upperText :: T.Text -> T.Text
upperText = T.map toUpper

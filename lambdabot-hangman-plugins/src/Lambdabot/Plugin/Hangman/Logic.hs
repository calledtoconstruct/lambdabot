{-# LANGUAGE DeriveGeneric #-}

module Lambdabot.Plugin.Hangman.Logic where

import Data.List (sortOn, sort, sortBy, group, nub)
import Data.List.Split (splitOn)
import Data.Char (toUpper)
import Data.Aeson (ToJSON, toJSON, object, (.=))
import System.Random (mkStdGen, random)
import GHC.Generics

-- TODO:
-- [ ]: Add list of configuration to data type
-- [ ]: Add command for privileged user to add a phrase
-- [ ]: Add messages to configuration (You Win, You Lost, etc...)
-- [ ]: Intercalate letters already used
-- [ ]: Incorporate emoji

data Game =
  NoGame Configuration
  | InGame GameState Configuration
  deriving (Generic, Show, Read)

data GameState = GameState {
  userLetters :: String,
  correctLetters :: String,
  incorrectLetters :: String,
  target :: String
}
  deriving (Generic, Show, Read)

data Configuration = Configuration {
  phrases :: [String],
  lastPhrase :: Int,
  allowedMisses :: Int
}
  deriving (Generic, Show, Read)

-- Current State:
-- Target: Twitch Love
-- User Letters: wcclaaabccccccc
-- Correct Letters: h
-- Incorrect Letters: z

-- Calculated:
-- Bad Guesses: z
-- Good Guesses: h
-- Popular Guess: c
-- Board: _ _ _ _ _ H   _ _ _ _
-- Man: Q________*
-- Man: Q_______*
-- Man: Q______*
-- Man: Q*
-- Man: KAA-POW!

incorrectArgumentsForStart :: String
incorrectArgumentsForStart = "Incorrect number of arguments, please do not add parameters to start command."

incorrectArgumentsForShow :: String
incorrectArgumentsForShow = "Incorrect number of arguments, please do not add parameters to show command."

incorrectArgumentsForProgress :: String
incorrectArgumentsForProgress = "Incorrect number of arguments, please do not add parameters to final answer command."

incorrectArgumentsForAppend :: String
incorrectArgumentsForAppend = "Incorrect number of arguments, please include the character you want to guess."

incorrectArgumentsForAddPhrase :: String
incorrectArgumentsForAddPhrase = "Incorrect number of arguments, please include the phrase you want to add."

incorrectArgumentsForRemovePhrase :: String
incorrectArgumentsForRemovePhrase = "Incorrect number of arguments, please include the phrase you want to remove."

noGameInProgress :: String
noGameInProgress = "No game is in progress, use ?hangman-start to start a new game."

gameInProgress :: String
gameInProgress = "A game is in progress, please complete this game before starting a new one."

messagePhraseAdded :: String
messagePhraseAdded = "Phrase added"

messagePhraseRemoved :: String
messagePhraseRemoved = "Phrase removed"

messageYouWon :: String
messageYouWon = "You win!"

messageYouLost :: String
messageYouLost = "You lost!"

messageThereWereNoGuesses :: String
messageThereWereNoGuesses = "There were no guesses!"

messageNewGameHasBegun :: String
messageNewGameHasBegun = "A new game of Hangman has begun!  Guess the first letter using:  ?hangman-guess [letter]"

messageIncorrectGuessesTried :: String
messageIncorrectGuessesTried = "You have already tried: [@]"

messageNumberOfGuessesRemaining :: String
messageNumberOfGuessesRemaining = "You have @ incorrect guesses left."

initializeGame :: Game -> ([String], Game)
initializeGame (NoGame configuration) = (message: [], updatedState)
    where updatedState = newGame phrase updatedConfiguration
          (updatedConfiguration, phrase) = selectPhrase configuration
          message = messageNewGameHasBegun
initializeGame game = ([gameInProgress], game)

selectPhrase :: Configuration -> (Configuration, String)
selectPhrase configuration@(Configuration options previous _) = (updatedConfiguration, phrase)
  where maximum = length options
        generator = mkStdGen previous
        (selected, _) = random generator
        phrase = options !! (selected `mod` maximum)
        updatedConfiguration = configuration { lastPhrase = selected }

newGame :: String -> Configuration -> Game
newGame answer = InGame (GameState {
  target = answer,
  userLetters = "",
  correctLetters = "",
  incorrectLetters = ""
})

addGuess :: Game -> Char -> ([String], Game)
addGuess game letter = ([], finalState)
  where finalState = modifyState game $ toUpper letter

progressGame :: Game -> ([String], Game)
progressGame (NoGame configuration) = ([noGameInProgress], NoGame configuration)
progressGame game =
  case getMostPopularGuess game of
    Just guess -> applyGuess game guess
    Nothing -> ([messageThereWereNoGuesses], game)

applyGuess :: Game -> Char -> ([String], Game)
applyGuess (NoGame configuration) _ = ([noGameInProgress], NoGame configuration)
applyGuess (InGame gameState configuration) popular = (messages, updatedGame)
  where correct = isCorrect gameState popular
        result = case correct of
          Just True -> "correct"
          Just False -> "incorrect"
          Nothing -> "already guessed"
        outcome = "The popular guess was " ++ popular: " and that was " ++ result ++ "."
        updatedGameState = updateGameState gameState popular correct
        (evaluation, maybeGameState) = evaluateGame updatedGameState configuration
        updatedGame = case maybeGameState of
          Nothing -> NoGame configuration
          Just finalGameState -> InGame finalGameState configuration
        messages = outcome: evaluation

evaluateGame :: GameState -> Configuration -> ([String], Maybe GameState)
evaluateGame gameState@(GameState _ correct incorrect answer) configuration
  | remaining <= 0 = ([board, messageYouLost], Nothing)
  | length correct == length (nub $ filter (/= ' ') answer) = ([board, messageYouWon], Nothing)
  | otherwise = (board: guesses, Just gameState)
  where remaining = calculateRemaining configuration incorrect
        guesses = showGuesses gameState configuration
        board = showBoard gameState

showGuesses :: GameState -> Configuration -> [String]
showGuesses (GameState _ _ incorrect _) configuration = [remainingGuesses, incorrectGuesses]
  where remaining = calculateRemaining configuration incorrect
        remainingGuesses = substituteRemaining $ show remaining
        incorrectGuesses = substituteIncorrect incorrect

substituteIncorrect :: String -> String
substituteIncorrect value = concat $ [prefix, value, suffix]
  where (prefix: suffix: []) = splitOn "@" messageIncorrectGuessesTried

substituteRemaining :: String -> String
substituteRemaining value = concat $ [prefix, value, suffix]
  where (prefix: suffix: []) = splitOn "@" messageNumberOfGuessesRemaining

calculateRemaining :: Configuration -> String -> Int
calculateRemaining (Configuration _ _ allowedMisses) incorrect = allowedMisses - length incorrect

getMostPopularGuess :: Game -> Maybe Char
getMostPopularGuess (InGame (GameState letters _ _ _) _)
  | null letters = Nothing
  | otherwise = Just $ last $ concat $ sortOn length $ group $ sort letters

isCorrect :: GameState -> Char -> Maybe Bool
isCorrect (GameState _ correct _ answer) letter = case foundInTarget of
  True -> case foundInCorrectLetters of
    True -> Nothing
    False -> Just True
  False -> Just False
  where foundInTarget = elem letter answer
        foundInCorrectLetters = elem letter correct

updateGameState :: GameState -> Char -> Maybe Bool -> GameState
updateGameState (GameState _ correct incorrect answer) letter (Just True) = GameState {
  userLetters = [],
  correctLetters = letter: correct,
  incorrectLetters = incorrect,
  target = answer
}
updateGameState (GameState _ correct incorrect answer) letter (Just False) = GameState {
  userLetters = [],
  correctLetters = correct,
  incorrectLetters = letter: incorrect,
  target = answer
}
updateGameState (GameState _ correct incorrect answer) letter Nothing = GameState {
  userLetters = [],
  correctLetters = correct,
  incorrectLetters = letter: incorrect,
  target = answer
}

modifyState :: Game -> Char -> Game
modifyState (NoGame configuration) _ = NoGame configuration
modifyState (InGame game configuration) char = InGame game { userLetters = char: userLetters game } configuration

showGame :: Game -> [String]
showGame (NoGame _) = [noGameInProgress]
showGame (InGame gameState configuration) = board: guesses
    where board = showBoard gameState
          guesses = showGuesses gameState configuration

showBoard :: GameState -> String
showBoard (GameState _ correct _ answer) = output
  where output = intercalate '.' board
        board = map (transformLetter correct) answer

showInternalState :: Game -> String
showInternalState (NoGame _) = noGameInProgress
showInternalState (InGame (GameState user correct incorrect answer) _) = output
    where output = user ++ ":" ++ correct ++ ":" ++ incorrect ++ ":" ++ answer

transformLetter :: [Char] -> Char -> Char
transformLetter correct letter = case elem letter $ ' ': correct of
  True -> letter
  False -> '_'

intercalate :: Char -> [Char] -> [Char]
intercalate _ [] = []
intercalate value (first: rest)
  | null ending = first: ending
  | otherwise = first: value: ending
  where ending = intercalate value rest

addPhrase :: Game -> String -> Game
addPhrase (NoGame configuration) phrase = NoGame $ addPhrase' configuration phrase
addPhrase (InGame gameState configuration) phrase = InGame gameState $ addPhrase' configuration phrase

addPhrase' :: Configuration -> String -> Configuration
addPhrase' configuration@(Configuration options _ _) phrase = configuration { phrases = upperPhrase: options }
  where upperPhrase = map toUpper phrase

removePhrase :: Game -> String -> Game
removePhrase (NoGame configuration) phrase = NoGame $ removePhrase' configuration phrase
removePhrase (InGame gameState configuration) phrase = InGame gameState $ removePhrase' configuration phrase

removePhrase' :: Configuration -> String -> Configuration
removePhrase' configuration@(Configuration options _ _) phrase = configuration { phrases = filtered }
  where filtered = filter (/= upperPhrase) options
        upperPhrase = map toUpper phrase

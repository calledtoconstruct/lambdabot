{-# LANGUAGE DeriveGeneric #-}

module Lambdabot.Plugin.Hangman.Logic where

import Data.List (sortOn, sort, sortBy, group, nub)
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
  lastPhrase :: Int
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

noGameInProgress :: String
noGameInProgress = "No game is in progress, use ?hangman-start to start a new game."

gameInProgress :: String
gameInProgress = "A game is in progress, please complete this game before starting a new one."

initializeGame :: Game -> ([String], Game)
initializeGame (NoGame configuration) = (message: [], updatedState)
    where updatedState = newGame phrase updatedConfiguration
          (updatedConfiguration, phrase) = selectPhrase configuration
          message = "The hangman state has been reset."
initializeGame game = ([gameInProgress], game)

selectPhrase :: Configuration -> (Configuration, String)
selectPhrase configuration@(Configuration options previous) = (updatedConfiguration, phrase)
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
    Nothing -> (["There were no guesses!"], game)

applyGuess :: Game -> Char -> ([String], Game)
applyGuess previousState popular = (messages, finalState)
  where correct = isCorrect previousState popular
        result = case correct of
          Just True -> "correct"
          Just False -> "incorrect"
          Nothing -> "already guessed"
        outcome = "The popular guess was " ++ popular: " and that was " ++ result ++ "."
        updatedState = saveFinalAnswer previousState popular correct
        board = showBoard updatedState
        (game, finalState) = evaluateGame updatedState
        messages = outcome: board: game

allowedMisses :: Int
allowedMisses = 10

youWon :: String
youWon = "You win!"

youLost :: String
youLost = "You lost!"

evaluateGame :: Game -> ([String], Game)
evaluateGame (NoGame configuration) = ([noGameInProgress], NoGame configuration)
evaluateGame game@(InGame (GameState _ correct incorrect answer) configuration)
  | remaining <= 0 = ([youLost], NoGame configuration)
  | length correct == length (nub $ filter (/= ' ') answer) = ([youWon], NoGame configuration)
  | otherwise = ([remainingGuesses, incorrectGuesses], game)
  where remaining = allowedMisses - length incorrect
        remainingGuesses = "You have " ++ show remaining ++ " incorrect guesses left."
        incorrectGuesses = "You have already tried: " ++ incorrect

getMostPopularGuess :: Game -> Maybe Char
getMostPopularGuess (InGame (GameState letters _ _ _) _)
  | null letters = Nothing
  | otherwise = Just $ last $ concat $ sortOn length $ group $ sort letters

isCorrect :: Game -> Char -> Maybe Bool
isCorrect (InGame (GameState _ correct _ answer) _) letter = case foundInTarget of
  True -> case foundInCorrectLetters of
    True -> Nothing
    False -> Just True
  False -> Just False
  where foundInTarget = elem letter answer
        foundInCorrectLetters = elem letter correct

saveFinalAnswer :: Game -> Char -> Maybe Bool -> Game
saveFinalAnswer (InGame gameState configuration) letter action = InGame updatedGameState configuration
  where updatedGameState = updateGameState gameState letter action
saveFinalAnswer (NoGame configuration) _ _ = NoGame configuration

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

showBoard :: Game -> String
showBoard (NoGame _) = "No game in progress at the moment.  Use ?hangman-start to start one."
showBoard (InGame (GameState _ correct _ answer) _) = output
  where output = intercalate '.' board
        board = map (transformLetter correct) answer

showInternalState :: Game -> String
showInternalState (NoGame _) = "No game in progress at the moment.  Use ?hangman-start to start one."
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

module Lambdabot.Plugin.Hangman.Logic where

import Data.List (sortOn, sort, sortBy, group, nub)
import Data.Char (toUpper)

data Game =
  NoGame
  | InGame GameState

data GameState = GameState {
  userLetters :: String,
  correctLetters :: String,
  incorrectLetters :: String,
  target :: String
}

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
initializeGame NoGame = (message: [], updatedState)
    where updatedState = newGame "ANGEL FISH"
          message = "The hangman state has been reset."
initializeGame game = (gameInProgress: [], game)

newGame :: String -> Game
newGame target = InGame $ GameState {
  target = target,
  userLetters = "",
  correctLetters = "",
  incorrectLetters = ""
}

addGuess :: Game -> Char -> ([String], Game)
addGuess game letter = ([], finalState)
  where finalState = modifyState game $ toUpper letter

progressGame :: Game -> ([String], Game)
progressGame NoGame = ([noGameInProgress], NoGame)
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
evaluateGame NoGame = ([noGameInProgress], NoGame)
evaluateGame game@(InGame (GameState userLetters correctLetters incorrectLetters target))
  | remaining <= 0 = ([youLost], NoGame)
  | (length correctLetters) == (length $ nub $ filter (/= ' ') target) = ([youWon], NoGame)
  | otherwise = (remainingGuesses: incorrectGuesses: [], game)
  where remaining = allowedMisses - (length incorrectLetters)
        remainingGuesses = "You have " ++ (show remaining) ++ " incorrect guesses left."
        incorrectGuesses = "You have already tried: " ++ incorrectLetters

getMostPopularGuess :: Game -> Maybe Char
getMostPopularGuess (InGame (GameState letters _ _ _))
  | null letters = Nothing
  | otherwise = Just $ last $ concat $ (sortOn length) $ group $ sort letters

isCorrect :: Game -> Char -> Maybe Bool
isCorrect (InGame (GameState _ correctLetters _ target)) letter = case foundInTarget of
  True -> case foundInCorrectLetters of
    True -> Nothing
    False -> Just True
  False -> Just False
  where foundInTarget = elem letter target
        foundInCorrectLetters = elem letter correctLetters

saveFinalAnswer :: Game -> Char -> Maybe Bool -> Game
saveFinalAnswer (InGame gameState) letter action = InGame $ updateGameState gameState letter action
saveFinalAnswer NoGame _ _ = NoGame

updateGameState :: GameState -> Char -> Maybe Bool -> GameState
updateGameState (GameState _ correctLetters incorrectLetters target) letter (Just True) = GameState {
  userLetters = [],
  correctLetters = letter: correctLetters,
  incorrectLetters = incorrectLetters,
  target = target
}
updateGameState (GameState _ correctLetters incorrectLetters target) letter (Just False) = GameState {
  userLetters = [],
  correctLetters = correctLetters,
  incorrectLetters = letter: incorrectLetters,
  target = target
}
updateGameState (GameState _ correctLetters incorrectLetters target) letter Nothing = GameState {
  userLetters = [],
  correctLetters = correctLetters,
  incorrectLetters = letter: incorrectLetters,
  target = target
}

modifyState :: Game -> Char -> Game
modifyState NoGame _ = NoGame
modifyState (InGame game) char = (InGame game { userLetters = char: userLetters game })

showBoard :: Game -> String
showBoard NoGame = "No game in progress at the moment.  Use ?hangman-start to start one."
showBoard (InGame (GameState _ correctLetters _ target)) = output
  where output = concatMap (: ".") board
        board = map (transformLetter correctLetters) target

showInternalState :: Game -> String
showInternalState NoGame = "No game in progress at the moment.  Use ?hangman-start to start one."
showInternalState (InGame (GameState userLetters correctLetters incorrectLetters target)) = output
    where output = userLetters ++ ":" ++ correctLetters ++ ":" ++ incorrectLetters ++ ":" ++ target

transformLetter :: [Char] -> Char -> Char
transformLetter correctLetters letter = case elem letter $ ' ': correctLetters of
  True -> letter
  False -> '_'

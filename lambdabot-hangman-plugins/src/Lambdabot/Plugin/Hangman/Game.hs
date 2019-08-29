{-# LANGUAGE DeriveGeneric #-}

module Lambdabot.Plugin.Hangman.Game (
  Result (Result),
  game,
  messages,
  Game (NoGame, InGame),
  newGame,
  getConfiguration,
  initializeGame,
  showGame,
  showBoard,
  showGuesses,
  calculateRemaining,
  GameState (GameState),
  userLetters,
  correctLetters,
  incorrectLetters,
  target,
  Messages,
  gameInProgress,
  noGameInProgress,
  incorrectArgumentsForStart,
  incorrectArgumentsForShow,
  incorrectArgumentsForProgress,
  incorrectArgumentsForAppend,
  incorrectArgumentsForAddPhrase,
  incorrectArgumentsForRemovePhrase,
  messageIncorrectArgumentsForConfigure,
  messageUnknownConfigurationOption,
  messageConfigurationUpdated
  ) where

import GHC.Generics
import Data.Universe.Helpers ((+++))
import Data.List.Split (splitOn)

import Lambdabot.Plugin.Hangman.Configuration

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

type Messages = [String]

data Result = Result {
  messages :: Messages,
  game :: Game
}

newGame :: String -> Configuration -> Game
newGame answer = InGame (GameState {
  target = answer,
  userLetters = "",
  correctLetters = "",
  incorrectLetters = ""
})

getConfiguration :: Game -> Configuration
getConfiguration (NoGame configuration) = configuration
getConfiguration (InGame _ configuration) = configuration

initializeGame :: Game -> Result
initializeGame (NoGame configuration) = Result output updatedGame
    where updatedGame = newGame phrase updatedConfiguration
          (updatedConfiguration, phrase) = selectPhrase configuration
          begun = messageNewGameHasBegun configuration
          output = (:) begun $ showGame updatedGame
initializeGame previous = Result [gameInProgress] previous

showGame :: Game -> Messages
showGame (NoGame _) = [noGameInProgress]
showGame (InGame gameState configuration) = board: guesses
    where board = showBoard gameState configuration
          guesses = showGuesses gameState configuration

showBoard :: GameState -> Configuration -> String
showBoard (GameState _ correct _ answer) configuration = message
  where message = substituteTokens (messageGuessing configuration) "@" [board]
        board = intercalate '.' boardState
        boardState = map (transformLetter correct) answer

showGuesses :: GameState -> Configuration -> Messages
showGuesses gameState configuration = [remainingGuesses, incorrectGuesses]
  where remaining = calculateRemaining configuration incorrect
        remainingGuesses = substituteTokens (messageNumberOfGuessesRemaining configuration) "@" $ [numberOfGuessesRemaining]
        incorrectGuesses = substituteTokens (messageIncorrectGuessesTried configuration) "@" [incorrect]
        numberOfGuessesRemaining = show remaining
        incorrect = incorrectLetters gameState

calculateRemaining :: Configuration -> String -> Int
calculateRemaining configuration incorrect = (allowedMisses configuration) - length incorrect

substituteTokens :: String -> String -> [String] -> String
substituteTokens template token values = message
  where segments = splitOn token template
        message = concat $ (+++) segments values

transformLetter :: [Char] -> Char -> Char
transformLetter correctGuesses letter = if isGuessed || isSpace then letter else '_'
  where isGuessed = elem letter correctGuesses
        isSpace = letter == ' '

intercalate :: Char -> [Char] -> [Char]
intercalate _ [] = []
intercalate value (first: rest)
  | null ending = first: ending
  | otherwise = first: value: ending
  where ending = intercalate value rest

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

messageIncorrectArgumentsForConfigure :: String
messageIncorrectArgumentsForConfigure = "Incorrect number of arguments, please use ?hangman-configure [option] [value]."

messageUnknownConfigurationOption :: String
messageUnknownConfigurationOption = "Unknown configuration option"

messageConfigurationUpdated :: String
messageConfigurationUpdated = "Configuration updated"

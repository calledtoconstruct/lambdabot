module Lambdabot.Plugin.Hangman.Logic where

import Lambdabot.Plugin.Hangman.Configuration (
  Configuration (
    messageAlreadyGuessed,
    messageCorrect,
    messageIncorrect,
    messageOutcome,
    messageThereWereNoGuesses,
    messageYouLost,
    messageYouWon
  ),
  validCharacters,
 )
import Lambdabot.Plugin.Hangman.Game (
  Game (..),
  GameState (correctLetters, incorrectLetters, target, userLetters),
  Messages,
  Result (Result),
  calculateRemaining,
  noGameInProgress,
  showBoard,
  showGuesses,
  substituteTokens,
 )

import Data.Char (toUpper)
import Data.List (group, nub, sort, sortOn)

addGuess :: Game -> Char -> Result
addGuess previous letter = if isValid then Result [] finalState else Result [messageNotRecognized] previous
 where
  finalState = modifyState previous upperLetter
  isValid = upperLetter `elem` validCharacters
  messageNotRecognized = ['[', upperLetter, ']'] ++ " is not recognized."
  upperLetter = toUpper letter

modifyState :: Game -> Char -> Game
modifyState (NoGame configuration) _ = NoGame configuration
modifyState (InGame previous configuration) char = InGame previous{userLetters = char : userLetters previous} configuration

progressGame :: Game -> Result
progressGame (NoGame configuration) = Result [noGameInProgress] $ NoGame configuration
progressGame previous@(InGame gameState configuration) =
  case getMostPopularGuess gameState of
    Just guess -> applyGuess gameState configuration guess
    Nothing -> Result [messageThereWereNoGuesses configuration] previous

applyGuess :: GameState -> Configuration -> Char -> Result
applyGuess gameState configuration popular = Result output updatedGame
 where
  correct = isCorrect gameState popular
  result = case correct of
    Correct -> messageCorrect configuration
    Incorrect -> messageIncorrect configuration
    AlreadyGuessed -> messageAlreadyGuessed configuration
  outcome = substituteTokens (messageOutcome configuration) "@" [[popular], result]
  updatedGameState = updateGameState gameState popular correct
  (evaluation, maybeGameState) = evaluateGame updatedGameState configuration
  updatedGame = case maybeGameState of
    Nothing -> NoGame configuration
    Just finalGameState -> InGame finalGameState configuration
  output = outcome : evaluation

evaluateGame :: GameState -> Configuration -> (Messages, Maybe GameState)
evaluateGame gameState configuration
  | remaining <= 0 = ([board, messageYouLost configuration], Nothing)
  | length (correctLetters gameState) == length (nub $ filter (/= ' ') (target gameState)) = ([board, messageYouWon configuration], Nothing)
  | otherwise = (board : guesses, Just gameState)
 where
  remaining = calculateRemaining configuration $ incorrectLetters gameState
  guesses = showGuesses gameState configuration
  board = showBoard gameState configuration

getMostPopularGuess :: GameState -> Maybe Char
getMostPopularGuess gameState
  | null letters = Nothing
  | otherwise = Just $ last $ concat $ sortOn length $ group $ sort letters
 where
  letters = userLetters gameState

data MaybeCorrect = AlreadyGuessed | Incorrect | Correct

isCorrect :: GameState -> Char -> MaybeCorrect
isCorrect gameState letter
  | alreadyGuessed = AlreadyGuessed
  | foundInTarget = Correct
  | otherwise = Incorrect
 where
  foundInTarget = elem letter $ target gameState
  alreadyGuessed = elem letter $ correctLetters gameState

updateGameState :: GameState -> Char -> MaybeCorrect -> GameState
updateGameState gameState letter Correct =
  gameState
    { userLetters = []
    , correctLetters = letter : correctLetters gameState
    }
updateGameState gameState letter _ =
  gameState
    { userLetters = []
    , incorrectLetters = letter : incorrectLetters gameState
    }

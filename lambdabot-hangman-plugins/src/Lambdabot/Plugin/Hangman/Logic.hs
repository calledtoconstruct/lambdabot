{-# LANGUAGE DeriveGeneric #-}

module Lambdabot.Plugin.Hangman.Logic where

import Lambdabot.Plugin.Hangman.Configuration
import Lambdabot.Plugin.Hangman.Game

import Data.List (sortOn, sort, group, nub)
import Data.Char (toUpper)

addGuess :: Game -> Char -> Result
addGuess previous letter = if isValid then Result [] finalState else Result [messageNotRecognized] previous
  where finalState = modifyState previous upperLetter
        isValid = elem upperLetter validCharacters
        messageNotRecognized = ['[', upperLetter, ']'] ++ " is not recognized."
        upperLetter = toUpper letter

modifyState :: Game -> Char -> Game
modifyState (NoGame configuration) _ = NoGame configuration
modifyState (InGame previous configuration) char = InGame previous { userLetters = char: userLetters previous } configuration

progressGame :: Game -> Result
progressGame (NoGame configuration) = Result [noGameInProgress] $ NoGame configuration
progressGame previous@(InGame gameState configuration) =
  case getMostPopularGuess gameState of
    Just guess -> applyGuess gameState configuration guess
    Nothing -> Result [messageThereWereNoGuesses configuration] previous

applyGuess :: GameState -> Configuration -> Char -> Result
applyGuess gameState configuration popular = Result output updatedGame
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
        output = outcome: evaluation

evaluateGame :: GameState -> Configuration -> (Messages, Maybe GameState)
evaluateGame gameState configuration
  | remaining <= 0 = ([board, messageYouLost configuration], Nothing)
  | length (correctLetters gameState) == length (nub $ filter (/= ' ') (target gameState)) = ([board, messageYouWon configuration], Nothing)
  | otherwise = (board: guesses, Just gameState)
  where remaining = calculateRemaining configuration $ incorrectLetters gameState
        guesses = showGuesses gameState configuration
        board = showBoard gameState configuration

getMostPopularGuess :: GameState -> Maybe Char
getMostPopularGuess gameState
  | null letters = Nothing
  | otherwise = Just $ last $ concat $ sortOn length $ group $ sort letters
  where letters = userLetters gameState

isCorrect :: GameState -> Char -> Maybe Bool
isCorrect gameState letter = case foundInTarget of
  True -> case foundInCorrectLetters of
    True -> Nothing
    False -> Just True
  False -> Just False
  where foundInTarget = elem letter answer
        foundInCorrectLetters = elem letter correct
        correct = correctLetters gameState
        answer = target gameState

type MaybeCorrect = Maybe Bool

updateGameState :: GameState -> Char -> MaybeCorrect -> GameState
updateGameState gameState letter maybeIsCorrect
  | maybeIsCorrect == Just True = GameState {
    userLetters = [],
    correctLetters = letter: correct,
    incorrectLetters = incorrect,
    target = answer
  }
  | maybeIsCorrect == Just False = GameState {
    userLetters = [],
    correctLetters = correct,
    incorrectLetters = letter: incorrect,
    target = answer
  }
  | otherwise = GameState {
    userLetters = [],
    correctLetters = correct,
    incorrectLetters = letter: incorrect,
    target = answer
  }
  where correct = correctLetters gameState
        incorrect = incorrectLetters gameState
        answer = target gameState

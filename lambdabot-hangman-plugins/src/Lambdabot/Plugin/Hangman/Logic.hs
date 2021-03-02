{-# LANGUAGE OverloadedStrings #-}

module Lambdabot.Plugin.Hangman.Logic where

import Lambdabot.Plugin.Hangman.Configuration (
  Configuration (
    defaultSecondsBetweenCycles,
    messageAlreadyGuessed,
    messageCorrect,
    messageIncorrect,
    messageOutcome,
    messageThereWereNoGuesses,
    messageYouLost,
    messageYouWon
  ),
  Phrase,
  elemText,
  upperText,
  validCharacters,
 )
import Lambdabot.Plugin.Hangman.Game (
  Game (..),
  GameState (..),
  Messages,
  Result (Result),
  calculateRemaining,
  completeSetOfLetters,
  getMostPopularGuess,
  isSolved,
  noGameInProgress,
  showBoard,
  showGuesses,
  substituteTokens,
 )

import Data.Char (toUpper)
import qualified Data.Text as T

addGuess :: Game -> Char -> Result
addGuess previous letter = if isValid then Result [] finalState else Result [messageNotRecognized] previous
 where
  finalState = modifyState previous upperLetter
  isValid = upperLetter `elemText` validCharacters
  messageNotRecognized = T.cons '[' $ T.cons upperLetter $ T.cons ']' " is not recognized."
  upperLetter = toUpper letter

modifyState :: Game -> Char -> Game
modifyState (NoGame configuration) _ = NoGame configuration
modifyState (InGame previous configuration) char = InGame previous{userLetters = T.cons char $ userLetters previous} configuration

guessFinal :: Game -> Phrase -> Result
guessFinal (NoGame configuration) _ = Result [noGameInProgress] $ NoGame configuration
guessFinal (InGame gameState configuration) phrase =
  let correct = upperText (T.strip phrase) == target gameState
      updatedGameState =
        if correct
          then gameState{correctLetters = completeSetOfLetters gameState}
          else gameState{allowedMisses = allowedMisses gameState - 1}
   in let (evaluation', maybeGameState) = evaluateGame updatedGameState configuration
          evaluation =
            if correct
              then ("You are " `T.append` messageCorrect configuration `T.append` "!") : evaluation'
              else ("That guess is " `T.append` messageIncorrect configuration `T.append` ".") : evaluation'
          updatedGame = case maybeGameState of
            Nothing -> NoGame configuration
            Just finalGameState -> InGame finalGameState configuration
       in Result evaluation updatedGame

progressGame :: Game -> Result
progressGame (NoGame configuration) = Result [noGameInProgress] $ NoGame configuration
progressGame previous@(InGame gameState configuration) =
  case getMostPopularGuess gameState of
    Just guess -> applyGuess gameState{secondsToNextCycle = defaultSecondsBetweenCycles configuration} configuration guess
    Nothing -> Result [messageThereWereNoGuesses configuration] previous

applyGuess :: GameState -> Configuration -> Char -> Result
applyGuess gameState configuration popular = Result output updatedGame
 where
  correct = isCorrect gameState popular
  result = case correct of
    Correct -> messageCorrect configuration
    Incorrect -> messageIncorrect configuration
    AlreadyGuessed -> messageAlreadyGuessed configuration
  outcome = substituteTokens (messageOutcome configuration) "@" [T.singleton popular, result]
  updatedGameState = updateGameState gameState popular correct
  (evaluation, maybeGameState) = evaluateGame updatedGameState configuration
  updatedGame = case maybeGameState of
    Nothing -> NoGame configuration
    Just finalGameState -> InGame finalGameState configuration
  output = outcome : evaluation

evaluateGame :: GameState -> Configuration -> (Messages, Maybe GameState)
evaluateGame gameState configuration
  | remaining <= 0 = ([board, messageYouLost configuration], Nothing)
  | isSolved gameState = ([board, messageYouWon configuration], Nothing)
  | otherwise = (board : guesses, Just gameState)
 where
  remaining = calculateRemaining gameState $ incorrectLetters gameState
  guesses = showGuesses gameState configuration
  board = showBoard gameState configuration

data MaybeCorrect = AlreadyGuessed | Incorrect | Correct

isCorrect :: GameState -> Char -> MaybeCorrect
isCorrect gameState letter
  | alreadyGuessed = AlreadyGuessed
  | foundInTarget = Correct
  | otherwise = Incorrect
 where
  foundInTarget = elemText letter $ target gameState
  alreadyGuessed = elemText letter $ correctLetters gameState

updateGameState :: GameState -> Char -> MaybeCorrect -> GameState
updateGameState gameState letter Correct =
  gameState
    { userLetters = T.empty
    , correctLetters = T.cons letter $ correctLetters gameState
    }
updateGameState gameState letter _ =
  gameState
    { userLetters = T.empty
    , incorrectLetters = T.cons letter $ incorrectLetters gameState
    }

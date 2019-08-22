
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Lambdabot.Plugin.Hangman.Hangman (
  hangmanPlugin
) where

import Lambdabot.Plugin
import Lambdabot.Compat.PackedNick (packNick, unpackNick)

import qualified Data.ByteString.Char8 as P
import Data.List.Split -- (splitOn)
import Data.Ord
import Data.ByteString.Lazy (fromChunks, toChunks)
import Text.Read (readMaybe)

import Lambdabot.Plugin.Hangman.Logic

type HangmanState = Game
type Hangman = ModuleT HangmanState LB

hangmanPlugin :: Module HangmanState
hangmanPlugin = newModule {
  -- moduleSerialize = Just stdSerial,
  moduleDefState  = return NoGame,
  moduleInit      = return (),
  moduleCmds      = return [
    (command "hangman-start") {
      help = say "hangman-start - Starts the game.",
      process = clearState
    },
    (command "hangman-status") {
      help = say "hangman-status - Prints the current state of the game.",
      process = showState
    },
    (command "hangman-final-answer") {
      help = say "hangman-final-answer - Tallies the guesses and applies the most popular one.",
      process = progress
    },
    (command "hm") {
      help = say "hm [char] - Provide your guess.",
      process = appendState
    }
  ]
}

clearState :: String -> Cmd Hangman ()
clearState [] = 
  withMS $ \game writer -> do
    let (messages, updatedState) = initializeGame game
    writer updatedState
    sayMessages messages
clearState rest = say incorrectArgumentsForStart

showState :: String -> Cmd Hangman ()
showState [] = withMS $ \game _ -> say $ showBoard game
showState rest = say incorrectArgumentsForShow

progress :: String -> Cmd Hangman ()
progress [] =
  withMS $ \game writer -> do
    let (messages, updatedState) = progressGame game
    writer updatedState
    sayMessages messages
progress rest = say incorrectArgumentsForProgress

appendState :: String -> Cmd Hangman ()
appendState [] = say incorrectArgumentsForAppend
appendState (letter: _) =
  withMS $ \game writer -> do
    let (messages, updatedState) = addGuess game letter
    writer updatedState
    sayMessages messages

sayMessages :: [String] -> Cmd Hangman ()
sayMessages [] = return ()
sayMessages messages = foldl1 (>>) $ fmap say messages

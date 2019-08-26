
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Lambdabot.Plugin.Hangman.Hangman (
  hangmanPlugin
) where

import Lambdabot.Plugin (
  ModuleT
  , Cmd
  , moduleDefState
  , moduleSerialize
  , moduleInit
  , moduleCmds
  , command
  , help
  , process
  , aliases
  , say
  , privileged
  , Module
  , LB
  , newModule
  , stdSerial
  , withMS
  )
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
  moduleSerialize = Just stdSerial,
  moduleDefState  = return (NoGame (Configuration {
    phrases = [
      "MONKATOS",
      "TWITCH SINGS",
      "BEST STREAMER",
      "IN REAL LIFE",
      "SCIENCE AND TECHNOLOGY",
      "SOFTWARE ENGINEERING",
      "HASKELL RULEZ"
    ],
    lastPhrase = 0,
    allowedMisses = 10
  })),
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
    (command "hangman-guess") {
      aliases = ["hg"],
      help = say "hangman-guess [letter] - Provide your guess.",
      process = appendState
    },
    (command "hangman-add") {
      help = say "hangman-add [phrase] - Add a new phrase to the database.",
      process = commandAddPhrase,
      privileged = True
    },
    (command "hangman-remove") {
      help = say "hangman-remove [phrase] - Remove a phrase from the database.",
      process = commandRemovePhrase,
      privileged = True
    }
  ]
}

clearState :: String -> Cmd Hangman ()
clearState [] = 
  withMS $ \game writer -> do
    let (initialization, updatedGame) = initializeGame game
    writer updatedGame
    let game = showGame updatedGame
    sayMessages $ initialization ++ game
clearState _ = say incorrectArgumentsForStart

showState :: String -> Cmd Hangman ()
showState [] = withMS $ \game _ -> sayMessages $ showGame game
showState _ = say incorrectArgumentsForShow

progress :: String -> Cmd Hangman ()
progress [] =
  withMS $ \game writer -> do
    let (messages, updatedState) = progressGame game
    writer updatedState
    sayMessages messages
progress _ = say incorrectArgumentsForProgress

appendState :: String -> Cmd Hangman ()
appendState [] = say incorrectArgumentsForAppend
appendState (letter: _) =
  withMS $ \game writer -> do
    let (messages, updatedState) = addGuess game letter
    writer updatedState
    sayMessages messages

sayMessages :: [String] -> Cmd Hangman ()
sayMessages [] = return ()
sayMessages messages = foldr1 (>>) $ fmap say messages

commandAddPhrase :: String -> Cmd Hangman ()
commandAddPhrase [] = say incorrectArgumentsForAddPhrase
commandAddPhrase phrase =
  withMS $ \game writer -> do
    writer $ addPhrase game phrase
    say "Phrase added"

commandRemovePhrase :: String -> Cmd Hangman ()
commandRemovePhrase [] = say incorrectArgumentsForRemovePhrase
commandRemovePhrase phrase =
  withMS $ \game writer -> do
    writer $ removePhrase game phrase
    say "Phrase removed"
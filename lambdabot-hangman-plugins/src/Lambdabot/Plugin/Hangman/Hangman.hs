
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

import Lambdabot.Plugin.Hangman.Configuration
import Lambdabot.Plugin.Hangman.Game
import Lambdabot.Plugin.Hangman.Logic
import Lambdabot.Plugin.Hangman.Manage

type HangmanState = Game
type Hangman = ModuleT HangmanState LB

hangmanPlugin :: Module HangmanState
hangmanPlugin = newModule {
  moduleSerialize = Just stdSerial,
  moduleDefState  = return (NoGame newConfiguration),
  moduleInit      = return (),
  moduleCmds      = return [
    (command "hangman-start") {
      help = say "hangman-start - Starts the game.",
      process = commandStartGame
    },
    (command "hangman-status") {
      help = say "hangman-status - Prints the current state of the game.",
      process = commandStatus
    },
    (command "hangman-final-answer") {
      help = say "hangman-final-answer - Tallies the guesses and applies the most popular one.",
      process = commandFinalAnswer
    },
    (command "hangman-guess") {
      aliases = ["hg"],
      help = say "hangman-guess [letter] - Provide your guess.",
      process = commandAppendGuess
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
    },
    (command "hangman-configure") {
      help = say "hangman-configure [option] [value] - Update the configuration option to the value provided.",
      process = commandConfigure,
      privileged = True
    }
  ]
}

commandStartGame :: String -> Cmd Hangman ()
commandStartGame [] = 
  withMS $ \previous writer -> do
    let result = initializeGame previous
    writer $ game result
    sayMessages $ messages result
commandStartGame _ = say incorrectArgumentsForStart

commandStatus :: String -> Cmd Hangman ()
commandStatus [] = withMS $ \current _ -> sayMessages $ showGame current
commandStatus _ = say incorrectArgumentsForShow

commandFinalAnswer :: String -> Cmd Hangman ()
commandFinalAnswer [] =
  withMS $ \previous writer -> do
    let result = progressGame previous
    writer $ game result
    sayMessages $ messages result
commandFinalAnswer _ = say incorrectArgumentsForProgress

commandAppendGuess :: String -> Cmd Hangman ()
commandAppendGuess [] = say incorrectArgumentsForAppend
commandAppendGuess (letter: _) =
  withMS $ \previous writer -> do
    let result = addGuess previous letter
    writer $ game result
    sayMessages $ messages result

commandAddPhrase :: String -> Cmd Hangman ()
commandAddPhrase [] = say incorrectArgumentsForAddPhrase
commandAddPhrase phrase =
  withMS $ \previous writer -> do
    let result = addPhrase previous phrase
    writer $ game result
    sayMessages $ messages result

commandRemovePhrase :: String -> Cmd Hangman ()
commandRemovePhrase [] = say incorrectArgumentsForRemovePhrase
commandRemovePhrase phrase =
  withMS $ \previous writer -> do
    let result = removePhrase previous phrase
    writer $ game result
    sayMessages $ messages result

commandConfigure :: String -> Cmd Hangman ()
commandConfigure [] = say messageIncorrectArgumentsForConfigure
commandConfigure input = 
  withMS $ \previous writer -> do
    let result = configure previous input
    writer $ game result
    sayMessages $ messages result

sayMessages :: Messages -> Cmd Hangman ()
sayMessages [] = return ()
sayMessages output = foldr1 (>>) $ fmap say output

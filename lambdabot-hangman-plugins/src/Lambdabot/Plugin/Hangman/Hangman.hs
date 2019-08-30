
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Lambdabot.Plugin.Hangman.Hangman (
  hangmanPlugin
) where

import Lambdabot.Monad (received)

import Lambdabot.Module (ircPersists)

import Lambdabot.IRC (
  IrcMessage (IrcMessage),
  ircMsgCommand,
  ircMsgLBName,
  ircMsgParams,
  ircMsgParams,
  ircMsgPrefix,
  ircMsgServer)

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
  , lb
  , newModule
  , stdSerial
  , withMS)
  
import Data.Map (member, insert, delete)
import Control.Monad (void, when, unless)
import Control.Monad.State (gets, modify)
import Control.Monad.Trans (lift)
import Control.Concurrent.Lifted (fork, threadDelay)
import System.Timeout.Lifted (timeout)

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
    lastPhrase = 0
  })),
  moduleInit      = startup,
  moduleCmds      = return [
    (command "hangman-start") {
      help = say "hangman-start - Starts the game.",
      process = clearState
    },
    (command "hangman-status") {
      aliases = ["hangman-state"],
      help = say "hangman-status - Prints the current state of the game.",
      process = showState
    },
    (command "hangman-final-answer") {
      help = say "hangman-final-answer - Tallies the guesses and applies the most popular one.",
      process = progress
    },
    (command "hangman-timer-tick") {
      help = say "hangman-timer-tick - For internal use only.",
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

startup :: Hangman ()
startup = withMS $ \game _ -> maybeStartTimer game

maybeStartTimer :: Game -> Hangman ()
maybeStartTimer (InGame _ _) = do
  _ <- fork timerLoop
  lift $ modify (\state -> state {
    ircPersists = insert "hangman-timer-loop" True $ ircPersists state
  })
  return ()
maybeStartTimer _ = return ()

timerLoop :: Hangman ()
timerLoop = do
  threadDelay $ 15 * 1000 * 1000
  run <- lift $ gets (member "hangman-timer-loop" . ircPersists)
  when run $ lb . void . timeout (15 * 1000 * 1000) . received $ IrcMessage {
    ircMsgServer  = "twitch",
    ircMsgLBName  = "swarmcollective",
    ircMsgPrefix  = "hiveworker!n=hiveworker@hiveworker.tmi.twitch.tv",
    ircMsgCommand = "PRIVMSG",
    ircMsgParams  = ["#swarmcollective", ":?hangman-timer-tick"]
  }
  when run $ do
    _ <- fork timerLoop
    return ()

clearState :: String -> Cmd Hangman ()
clearState [] =
  withMS $ \game writer -> do
    wasRunning <- lift $ lift $ gets (member "hangman-timer-loop" . ircPersists)
    let (messages, updatedState) = initializeGame game
    writer updatedState
    sayMessages messages
    unless wasRunning startTimer
clearState _ = say incorrectArgumentsForStart

startTimer :: Cmd Hangman ()
startTimer = do
  _ <- lift $ fork timerLoop
  lift $ lift $ modify (\state -> state {
    ircPersists = insert "hangman-timer-loop" True $ ircPersists state
  })

showState :: String -> Cmd Hangman ()
showState [] = withMS $ \game _ -> say $ showBoard game
showState _ = say incorrectArgumentsForShow

progress :: String -> Cmd Hangman ()
progress [] =
  withMS $ \game writer -> do
    let (messages, updatedState) = progressGame game
    writer updatedState
    sayMessages messages
    maybeStopTimer updatedState
progress _ = say incorrectArgumentsForProgress

maybeStopTimer :: Game -> Cmd Hangman ()
maybeStopTimer (NoGame _) = lift $ lift $ modify (\state -> state {
  ircPersists = delete "hangman-timer-loop" $ ircPersists state
})
maybeStopTimer (InGame _ _) = return ()

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
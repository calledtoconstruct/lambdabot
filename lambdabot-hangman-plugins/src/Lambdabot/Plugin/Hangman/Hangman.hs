{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Lambdabot.Plugin.Hangman.Hangman (hangmanPlugin) where

import Lambdabot.Config.Hangman (hangmanPhrases)
import Lambdabot.IRC (IrcMessage (IrcMessage), ircMsgCommand, ircMsgLBName, ircMsgParams, ircMsgPrefix, ircMsgServer)
import qualified Lambdabot.Message as Msg (channels, lambdabotName, nick, server)
import Lambdabot.Module (ircPersists)
import Lambdabot.Monad (received, send)
import Lambdabot.Plugin (
  Cmd,
  LB,
  Module,
  ModuleT,
  MonadConfig (getConfig),
  aliases,
  command,
  help,
  lb,
  moduleCmds,
  moduleDefState,
  moduleInit,
  moduleSerialize,
  newModule,
  privileged,
  process,
  readMS,
  say,
  stdSerial,
  withMS,
  withMsg,
 )
import Lambdabot.Plugin.Hangman.Configuration (newConfiguration)
import Lambdabot.Plugin.Hangman.Game (
  Game (..),
  GameState (botName, channel, initiator, server),
  Messages,
  Result (game, messages),
  incorrectArgumentsForAddPhrase,
  incorrectArgumentsForAppend,
  incorrectArgumentsForProgress,
  incorrectArgumentsForRemovePhrase,
  incorrectArgumentsForShow,
  incorrectArgumentsForStart,
  initializeGame,
  messageIncorrectArgumentsForConfigure,
  showGame,
 )
import Lambdabot.Plugin.Hangman.Logic (addGuess, progressGame)
import Lambdabot.Plugin.Hangman.Manage (addPhrase, configure, removePhrase)

import Control.Concurrent.Lifted (fork, threadDelay)
import Control.Monad (unless, void, when)
import Control.Monad.State (gets, modify)
import Control.Monad.Trans (lift)
import Data.Map (delete, insert, member)
import System.Timeout.Lifted (timeout)

type HangmanState = Game
type Hangman = ModuleT HangmanState LB

hangmanPlugin :: Module HangmanState
hangmanPlugin =
  newModule
    { moduleSerialize = Just stdSerial
    , moduleDefState = do
        phrases <- getConfig hangmanPhrases
        return $ NoGame $ newConfiguration phrases
    , moduleInit = startup
    , moduleCmds =
        return
          [ (command "hangman-start")
              { help = say "hangman-start - Starts the game."
              , process = commandStartGame
              }
          , (command "hangman-status")
              { aliases = ["hangman-state"]
              , help = say "hangman-status - Prints the current state of the game."
              , process = commandStatus
              }
          , (command "hangman-final-answer")
              { help = say "hangman-final-answer - Tallies the guesses and applies the most popular one."
              , process = commandFinalAnswer
              }
          , (command "hangman-timer-tick")
              { help = say "hangman-timer-tick - For internal use only."
              , process = commandFinalAnswer
              }
          , (command "hangman-guess")
              { aliases = ["hg"]
              , help = say "hangman-guess [letter] - Provide your guess."
              , process = commandAppendGuess
              }
          , (command "hangman-add")
              { help = say "hangman-add [phrase] - Add a new phrase to the database."
              , process = commandAddPhrase
              , privileged = True
              }
          , (command "hangman-remove")
              { help = say "hangman-remove [phrase] - Remove a phrase from the database."
              , process = commandRemovePhrase
              , privileged = True
              }
          , (command "hangman-configure")
              { help = say "hangman-configure [option] [value] - Update the configuration option to the value provided."
              , process = commandConfigure
              , privileged = True
              }
          ]
    }

startup :: Hangman ()
startup = withMS $ \current _ -> maybeStartTimer current

maybeStartTimer :: Game -> Hangman ()
maybeStartTimer (InGame _ _) = do
  _ <- fork timerLoop
  lift $
    modify
      ( \state ->
          state
            { ircPersists = insert "hangman-timer-loop" True $ ircPersists state
            }
      )
  return ()
maybeStartTimer _ = return ()

timerLoop :: Hangman ()
timerLoop = do
  delayThen sendReply 10 "5 seconds left"
  delayThen sendReply 2 "3 seconds"
  delayThen sendReply 1 "2 seconds"
  delayThen sendReply 1 "1 seconds"
  delayThen receiveCommand 1 "?hangman-timer-tick"
  run <- lift $ gets (member "hangman-timer-loop" . ircPersists)
  when run $ void $ fork timerLoop

delayThen :: (IrcMessage -> Hangman ()) -> Int -> String -> Hangman ()
delayThen sendOrReceive delay text = do
  threadDelay $ delay * 1000 * 1000
  run <- lift $ gets (member "hangman-timer-loop" . ircPersists)
  currentGame <- readMS
  when run $ case gameInProgress' currentGame of
    Just gameState ->
      sendOrReceive $
        IrcMessage
          { ircMsgServer = server gameState
          , ircMsgLBName = initiator gameState
          , ircMsgPrefix = botName gameState ++ "!n=" ++ botName gameState ++ "@" ++ botName gameState ++ ".tmi.twitch.tv"
          , ircMsgCommand = "PRIVMSG"
          , ircMsgParams = [channel gameState, ":" ++ text]
          }
    Nothing -> return ()

receiveCommand :: IrcMessage -> Hangman ()
receiveCommand = lb . void . timeout (10 * 1000 * 1000) . received

sendReply :: IrcMessage -> Hangman ()
sendReply = lift . send

gameInProgress' :: Game -> Maybe GameState
gameInProgress' (NoGame _) = Nothing
gameInProgress' (InGame gameState _) = Just gameState

commandStartGame :: String -> Cmd Hangman ()
commandStartGame [] =
  withMS $ \previous writer -> do
    wasRunning <- lift $ lift $ gets (member "hangman-timer-loop" . ircPersists)
    withMsg $ \msg -> do
      let result = initializeGame previous (Msg.server msg) (head $ Msg.channels msg) (Msg.nick msg) (Msg.lambdabotName msg)
      writer $ game result
      sayMessages $ messages result
      unless wasRunning startTimer
commandStartGame _ = say incorrectArgumentsForStart

startTimer :: Cmd Hangman ()
startTimer = do
  _ <- lift $ fork timerLoop
  lift $
    lift $
      modify
        ( \state ->
            state
              { ircPersists = insert "hangman-timer-loop" True $ ircPersists state
              }
        )

commandStatus :: String -> Cmd Hangman ()
commandStatus [] = withMS $ \current _ -> sayMessages $ showGame current
commandStatus _ = say incorrectArgumentsForShow

commandFinalAnswer :: String -> Cmd Hangman ()
commandFinalAnswer [] =
  withMS $ \previous writer -> do
    let result = progressGame previous
    writer $ game result
    sayMessages $ messages result
    maybeStopTimer $ game result
commandFinalAnswer _ = say incorrectArgumentsForProgress

maybeStopTimer :: Game -> Cmd Hangman ()
maybeStopTimer (NoGame _) =
  lift $
    lift $
      modify
        ( \state ->
            state
              { ircPersists = delete "hangman-timer-loop" $ ircPersists state
              }
        )
maybeStopTimer (InGame _ _) = return ()

commandAppendGuess :: String -> Cmd Hangman ()
commandAppendGuess [] = say incorrectArgumentsForAppend
commandAppendGuess (letter : _) =
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

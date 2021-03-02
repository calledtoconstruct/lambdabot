{-# LANGUAGE OverloadedStrings #-}

module Lambdabot.Plugin.Hangman.Background (maybeStartTimer, signalShutdown) where

import Lambdabot.IRC (IrcMessage (IrcMessage), MessageDirection (Outbound), ircDirection, ircMsgCommand, ircMsgLBName, ircMsgParams, ircMsgPrefix, ircMsgServer, ircTags)
import Lambdabot.Module (ircPersists)
import Lambdabot.Monad (received, send)
import Lambdabot.Plugin (lb, withMS)
import Lambdabot.Plugin.Hangman.Configuration (Configuration (defaultSecondsBetweenCycles), Message)
import Lambdabot.Plugin.Hangman.Game (Game (..), GameState (..), Hangman)

import Control.Concurrent.Lifted (fork, threadDelay)
import Control.Monad (unless, void, when)
import Control.Monad.State (gets, modify)
import Control.Monad.Trans (lift)
import Data.Map (delete, insert, member)
import qualified Data.Text as T
import System.Timeout.Lifted (timeout)

maybeStartTimer :: [Game] -> Hangman ()
maybeStartTimer games =
  let atLeastOneGameIsActive = any gameIsActive games
   in do
        notAlreadyRunning <- not <$> isTimerLoopEnabled
        when (notAlreadyRunning && atLeastOneGameIsActive) $ do
          enableTimerLoop
          void $ fork timerLoop

timerLoop :: Hangman ()
timerLoop = do
  threadDelay (1 * 1000 * 1000)
  shutdown <- not <$> isTimerLoopEnabled
  unless shutdown $
    withMS $ \hangmanState write -> do
      let (updatedState, responses, continue) = foldl decrement ([], [], False) hangmanState
      write updatedState
      void $ fork $ mapM_ (\(f, m) -> f m) responses
      if continue
        then void $ fork timerLoop
        else signalShutdown

enableTimerLoop :: Hangman ()
enableTimerLoop = lift $ modify (\state -> state{ircPersists = insert "hangman-timer-loop" True $ ircPersists state})

isTimerLoopEnabled :: Hangman Bool
isTimerLoopEnabled = lift $ gets (member "hangman-timer-loop" . ircPersists)

signalShutdown :: Hangman ()
signalShutdown = lift $ modify (\state -> state{ircPersists = delete "hangman-timer-loop" $ ircPersists state})

decrement :: ([Game], [(IrcMessage -> Hangman (), IrcMessage)], Bool) -> Game -> ([Game], [(IrcMessage -> Hangman (), IrcMessage)], Bool)
decrement (games, ircMessages, _) (InGame gameState configuration) =
  let stnc = secondsToNextCycle gameState - 1
      sndWarn = [(sendReply, makeIrcMessage gameState (T.pack (show stnc) `T.append` " seconds")) | stnc `elem` warningAt gameState]
      recTick = [(receiveCommand, makeIrcMessage gameState "?hangman-timer-tick") | stnc == 0]
      stnc' = if stnc < 1 then defaultSecondsBetweenCycles configuration else stnc
      updatedGame = InGame gameState{secondsToNextCycle = stnc'} configuration
   in (updatedGame : games, sndWarn ++ recTick ++ ircMessages, True)
decrement (games, ircMessages, continue) (NoGame _) = (games, ircMessages, continue)

makeIrcMessage :: GameState -> Message -> IrcMessage
makeIrcMessage gameState text =
  IrcMessage
    { ircMsgServer = server gameState
    , ircMsgLBName = initiator gameState
    , ircMsgPrefix = botName gameState ++ "!n=" ++ botName gameState ++ "@" ++ botName gameState ++ ".tmi.twitch.tv"
    , ircMsgCommand = "PRIVMSG"
    , ircMsgParams = [channel gameState, ":" ++ T.unpack text]
    , ircDirection = Outbound
    , ircTags = []
    }

receiveCommand :: IrcMessage -> Hangman ()
receiveCommand = lb . void . timeout (10 * 1000 * 1000) . received

sendReply :: IrcMessage -> Hangman ()
sendReply = lift . send

gameIsActive :: Game -> Bool
gameIsActive (NoGame _) = False
gameIsActive (InGame _ _) = True
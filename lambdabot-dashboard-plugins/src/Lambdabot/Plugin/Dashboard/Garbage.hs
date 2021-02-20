module Lambdabot.Plugin.Dashboard.Garbage (startCollectingGarbage) where

import Lambdabot.Config.Dashboard (maximumNumberOfMessages, maximumNumberOfMessagesPerChannel)
import Lambdabot.Plugin.Dashboard.Configuration (Dashboard, DashboardState, Message (MkMessage), MessageUniqueIdentifier, Spoken (MkSpoken), Watcher (MkWatcher), WatcherUniqueIdentifier, messages, shutdown, speaking, watchers, watching, watchingFromChannel, uniqueIdentifierFromWatching, listOfMessageUniqueIdentifierFromSpoken)

import Lambdabot.Plugin (getConfig, readMS, withMS, MonadConfig)

import Control.Concurrent (threadDelay)
import Control.Concurrent.Lifted (fork)
import Control.Monad.Trans (liftIO)
import Data.Functor (void)
import Data.List.Unique (sortUniq)
import Control.Monad (when)

-- Note: This currently operates on a half-second cycle.  The interval provided is in seconds.

startCollectingGarbage :: Int -> Dashboard ()
startCollectingGarbage interval = void $ fork $ check (interval * 2) (interval * 2)

check :: Int -> Int -> Dashboard ()
check current interval =
  let cycleTime = 500 * 1000
      timeRemaining = current - 1
   in do
        liftIO $ threadDelay cycleTime
        continueRunning <- not . shutdown <$> readMS
        when continueRunning $ if timeRemaining == 0
          then do
            withMS takeOutTheGarbage
            void $ fork $ check interval interval
          else void $ fork $ check timeRemaining interval

takeOutTheGarbage :: MonadConfig m => DashboardState -> (DashboardState -> m b) -> m b
takeOutTheGarbage dashboardState writer = collectGarbage dashboardState
  <$> getConfig maximumNumberOfMessagesPerChannel
  <*> getConfig maximumNumberOfMessages
  >>= writer

collectGarbage :: DashboardState -> Int -> Int -> DashboardState
collectGarbage fullState maximumMessagesPerChannel maximumMessages =
  let everyoneWatching = sortUniq $ map uniqueIdentifierFromWatching $ concatMap watchingFromChannel $ watching fullState
      watchersStillWatching = filter (stillWatcher everyoneWatching) $ watchers fullState
      recentMessagesByChannel = map (limitMessages maximumMessagesPerChannel) $ speaking fullState
      recentMessages = concatMap listOfMessageUniqueIdentifierFromSpoken recentMessagesByChannel
      cleanMessages = take maximumMessages $ filter (isRecentMessage recentMessages) $ messages fullState
      relevantMessageUniqueIdentifiers = map getMessageUniqueIdentifier cleanMessages
      cleanSpoken = map (cleanSpokenInChannel relevantMessageUniqueIdentifiers) recentMessagesByChannel
   in fullState{messages = cleanMessages, watchers = watchersStillWatching, speaking = cleanSpoken}

isRecentMessage :: [MessageUniqueIdentifier] -> Message -> Bool
isRecentMessage recentMessages (MkMessage (mui, _, _, _)) = mui `elem` recentMessages

limitMessages :: Int -> Spoken -> Spoken
limitMessages maximumMessagesPerChannel (MkSpoken (channelName, messageUniqueIdentifiers)) = MkSpoken (channelName, take maximumMessagesPerChannel messageUniqueIdentifiers)

stillWatcher :: [WatcherUniqueIdentifier] -> Watcher -> Bool
stillWatcher everyoneWatching (MkWatcher (watcherUniqueIdentifier, _, _)) = watcherUniqueIdentifier `elem` everyoneWatching

getMessageUniqueIdentifier :: Message -> MessageUniqueIdentifier
getMessageUniqueIdentifier (MkMessage (messageUniqueIdentifier, _, _, _)) = messageUniqueIdentifier

cleanSpokenInChannel :: [MessageUniqueIdentifier] -> Spoken -> Spoken
cleanSpokenInChannel relevantMessages (MkSpoken (channelName, messagesSpokenInChannel)) = MkSpoken (channelName, filter (`elem` relevantMessages) messagesSpokenInChannel)

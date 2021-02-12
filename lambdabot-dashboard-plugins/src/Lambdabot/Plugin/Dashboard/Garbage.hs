module Lambdabot.Plugin.Dashboard.Garbage (startCollectingGarbage) where

import Lambdabot.Plugin.Dashboard.Configuration (Dashboard, DashboardState, messages, shutdown, watching, watchers, Watcher, WatcherUniqueIdentifier, Message, MessageUniqueIdentifier, speaking, Spoken)

import Lambdabot.Plugin (getConfig, readMS, withMS)

import Control.Concurrent (threadDelay)
import Control.Concurrent.Lifted (fork)
import Control.Monad.Trans (liftIO)
import Data.Functor (void)
import Lambdabot.Config.Dashboard (maximumNumberOfMessages)
import Data.List.Unique (sortUniq)

-- Note: This currently operates on a half-second cycle.  The interval provided is in seconds.

startCollectingGarbage :: Int -> Dashboard ()
startCollectingGarbage interval = void $ fork $ check (interval * 2) (interval * 2)

check :: Int -> Int -> Dashboard ()
check current interval =
  let cycleTime = 500 * 1000
      timeRemaining = current - 1
   in do
        maximumMessages <- getConfig maximumNumberOfMessages
        liftIO $ threadDelay cycleTime
        dashboardState <- readMS
        if not $ shutdown dashboardState
          then
            if timeRemaining == 0
              then withMS $ \full writer -> do
                writer $ collectGarbage full maximumMessages
                void $ fork $ check interval interval
              else void $ fork $ check timeRemaining interval
          else pure ()

collectGarbage :: DashboardState -> Int -> DashboardState
collectGarbage fullState maximumMessages =
  let everyoneWatching = sortUniq $ map fst $ concatMap snd $ watching fullState
      watchersStillWatching = filter (stillWatcher everyoneWatching) $ watchers fullState
      -- relevantMessages = filter (spokenBy everyoneWatching) $ messages fullState
      -- cleanMessages = take maximumMessages relevantMessages
      cleanMessages = take maximumMessages $ messages fullState
      relevantMessageUniqueIdentifiers = map getMessageUniqueIdentifier cleanMessages
      cleanSpoken = map (cleanSpokenInChannel relevantMessageUniqueIdentifiers) $ speaking fullState
   in fullState { messages = cleanMessages, watchers = watchersStillWatching, speaking = cleanSpoken }

stillWatcher :: [WatcherUniqueIdentifier] -> Watcher -> Bool
stillWatcher everyoneWatching (watcherUniqueIdentifier, _, _) = watcherUniqueIdentifier `elem` everyoneWatching

-- spokenBy :: [WatcherUniqueIdentifier] -> Message -> Bool
-- spokenBy everyoneWatching (_, watcherUniqueIdentifier, _, _) = watcherUniqueIdentifier `elem` everyoneWatching

getMessageUniqueIdentifier :: Message -> MessageUniqueIdentifier
getMessageUniqueIdentifier (messageUniqueIdentifier, _, _, _) = messageUniqueIdentifier

cleanSpokenInChannel :: [MessageUniqueIdentifier] -> Spoken -> Spoken
cleanSpokenInChannel relevantMessages (channelName, messagesSpokenInChannel) = (channelName, filter (`elem` relevantMessages) messagesSpokenInChannel)

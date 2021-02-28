{-# LANGUAGE OverloadedStrings #-}

module Lambdabot.Plugin.Dashboard.Service (startListening) where

import Lambdabot.Plugin.Dashboard.Configuration (Channel, ChannelName, Dashboard, DashboardState, Message (MkMessage), MessageUniqueIdentifier, Spoken, Watcher (MkWatcher), WatcherName, WatcherUniqueIdentifier, channelNameFromSpoken, listOfMessageUniqueIdentifierFromSpoken, messages, nameFromChannel, shutdown, speaking, uniqueIdentifierFromWatching, watchers, watching, watchingFromChannel, ChannelWatcher (MkChannelWatcher), nameFromWatcher, uniqueIdentifierFromWatcher, listOfBadgesFromWatching, Watching)

import Lambdabot.Logging (debugM)
import Lambdabot.Plugin (MonadLBState (withMS), readMS)
import Lambdabot.Util (io)

import Control.Concurrent.Lifted (ThreadId, fork, killThread, threadDelay)
import Control.Monad.Reader (liftIO)
import Data.Functor (void)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List.Unique (sortUniq)
import qualified Data.Text.Lazy as T
import Network.HTTP.Types.Status (status404)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (simpleCors)
import Network.Wai.Middleware.Gzip (def, gzip)
import Network.Wai.Middleware.Static (addBase, staticPolicy)
import Web.Scotty (get, json, param, scottyApp, text)
import Web.Scotty.Trans (capture, middleware, status)

startListening :: Int -> Dashboard ()
startListening port = do
  debugM "Starting Server..."

  ids <- readMS
  ioref <- liftIO $ newIORef ids

  threadId <-
    fork $
      io $ do
        app <- scottyApp $ do
          middleware simpleCors

          get (capture "/channel/:channel/watcher/list") $
            json =<< listWatcher
              <$> param "channel"
              <*> liftIO (readIORef ioref)

          get (capture "/channel/:channel/watcher/:watcher/list") $
            json =<< listWatcherChat
              <$> param "channel"
              <*> param "watcher"
              <*> liftIO (readIORef ioref)

          get (capture "/watcher/:watcher/channel/list") $
            json =<< listWatcherChannel
              <$> param "watcher"
              <*> liftIO (readIORef ioref)

          get (capture "/channel/:channel/chat/mode") $ do
            requestedChannel <- param "channel"
            status status404 *> text ("Not Implemented: " `T.append` T.pack requestedChannel)

          get (capture "/channel/:channel/chat/list") $
            json =<< listChat
              <$> param "channel"
              <*> liftIO (readIORef ioref)

          get (capture "/channel/list") $
            json . listChannels =<< liftIO (readIORef ioref)

        run port $ gzip def $ staticPolicy (addBase "html/") app

  void $ fork $ shutdownLoop threadId ioref

getWatchedChannel :: Channel -> ChannelName
getWatchedChannel = nameFromChannel

getSpokenChannel :: Spoken -> ChannelName
getSpokenChannel = channelNameFromSpoken

getChannelName :: Channel -> ChannelName
getChannelName = nameFromChannel

getWatcherName :: Watcher -> WatcherName
getWatcherName (MkWatcher (_, watcherName, _)) = watcherName

isRelevantMessage :: [MessageUniqueIdentifier] -> Message -> Bool
isRelevantMessage relevantMessages (MkMessage (messageUniqueIdentifier, _, _, _)) = messageUniqueIdentifier `elem` relevantMessages

isWatchingChannel :: ChannelName -> Channel -> Bool
isWatchingChannel requestedChannel = (==) requestedChannel . nameFromChannel

isSpeakingInChannel :: ChannelName -> Spoken -> Bool
isSpeakingInChannel requestedChannel = (==) requestedChannel . channelNameFromSpoken

isWatcherElemOf :: [WatcherUniqueIdentifier] -> Watcher -> Bool
isWatcherElemOf listOfWatchers (MkWatcher (watcherUniqueIdentifier, _, _)) = watcherUniqueIdentifier `elem` listOfWatchers

shutdownLoop :: ThreadId -> IORef DashboardState -> Dashboard ()
shutdownLoop threadId ioref = do
  threadDelay $ 333 * 1000
  withMS $ \dashboardState _ ->
    if shutdown dashboardState
      then do
        debugM "Killing listener thread."
        killThread threadId
      else do
        void $ liftIO $ writeIORef ioref dashboardState
        void $ fork $ shutdownLoop threadId ioref

listWatcherChat :: ChannelName -> WatcherUniqueIdentifier -> DashboardState -> [Message]
listWatcherChat requestedChannel watcherUniqueIdentifier dashboardState =
  let thisChannel = filter (isSpeakingInChannel requestedChannel) $ speaking dashboardState
   in if not $ null thisChannel
        then
          let messageUniqueIdentifiers = listOfMessageUniqueIdentifierFromSpoken $ head thisChannel
              relevantMessages = filter (isSpokenBy watcherUniqueIdentifier) $ filter (isRelevantMessage messageUniqueIdentifiers) $ messages dashboardState
           in relevantMessages
        else []

isSpokenBy :: WatcherUniqueIdentifier -> Message -> Bool
isSpokenBy watcherUniqueIdentifier (MkMessage (_, spokenByUniqueIdentifier, _, _)) = watcherUniqueIdentifier == spokenByUniqueIdentifier

listWatcherChannel :: WatcherUniqueIdentifier -> DashboardState -> [ChannelName]
listWatcherChannel watcherUniqueIdentifier dashboardState =
  let channelsTheyAreWatching = filter (isInChannel watcherUniqueIdentifier) $ watching dashboardState
   in map getChannelName channelsTheyAreWatching

isInChannel :: WatcherUniqueIdentifier -> Channel -> Bool
isInChannel watcherUniqueIdentifier thisChannel = elem watcherUniqueIdentifier $ map uniqueIdentifierFromWatching $ watchingFromChannel thisChannel

listWatcher :: ChannelName -> DashboardState -> [ChannelWatcher]
listWatcher requestedChannel dashboardState = do
  let watchersOfChannel = filter (isWatchingChannel requestedChannel) $ watching dashboardState
  if (not . null) watchersOfChannel
    then map (createChannelWatcher (watchers dashboardState)) $ watchingFromChannel $ head watchersOfChannel
    else []

createChannelWatcher :: [Watcher] -> Watching -> ChannelWatcher
createChannelWatcher allWatchers watchingThisChannel = MkChannelWatcher (watcherUniqueIdentifier, nameFromWatcher thisWatcher, listOfBadgesFromWatching watchingThisChannel)
  where watcherUniqueIdentifier = uniqueIdentifierFromWatching watchingThisChannel
        thisWatcher = head $ filter ((==) watcherUniqueIdentifier . uniqueIdentifierFromWatcher) allWatchers

listChat :: ChannelName -> DashboardState -> [Message]
listChat requestedChannel dashboardState =
  let thisChannel = filter (isSpeakingInChannel requestedChannel) $ speaking dashboardState
   in if not $ null thisChannel
        then
          let messageUniqueIdentifiers = listOfMessageUniqueIdentifierFromSpoken $ head thisChannel
              relevantMessages = filter (isRelevantMessage messageUniqueIdentifiers) $ messages dashboardState
           in relevantMessages
        else []

listChannels :: DashboardState -> [ChannelName]
listChannels dashboardState =
  let spokenChannels = map getSpokenChannel $ speaking dashboardState
      watchedChannels = map getWatchedChannel $ watching dashboardState
   in sortUniq $ spokenChannels ++ watchedChannels

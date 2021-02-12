module Lambdabot.Plugin.Dashboard.Service (startListening) where

import Lambdabot.Plugin.Dashboard.Configuration (Channel, ChannelName, Dashboard, DashboardState, Message, MessageUniqueIdentifier, Spoken, Watcher, WatcherName, WatcherUniqueIdentifier, messages, shutdown, speaking, watchers, watching)

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
import Network.Wai.Middleware.Gzip (gzip, def)
import Web.Scotty (get, json, param, scottyApp, text)
import Web.Scotty.Trans (capture, middleware, status)
import Network.Wai.Middleware.Static (addBase, staticPolicy)

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

              get (capture "/channel/:channel/watcher/list") $ do
                requestedChannel <- param (T.pack "channel")
                dashboardState <- liftIO $ readIORef ioref
                case listWatcher requestedChannel dashboardState of
                  Just watcherList -> json watcherList
                  Nothing -> status status404 *> text (T.pack "Channel Not Found")

              get (capture "/channel/:channel/watcher/:watcher/list") $ do
                requestedChannel <- param (T.pack "channel")
                requestedWatcher <- param (T.pack "watcher")
                dashboardState <- liftIO $ readIORef ioref
                case listWatcherChat requestedChannel requestedWatcher dashboardState of
                  Just watcherChatList -> json watcherChatList
                  Nothing -> status status404 *> text (T.pack "Channel Not Found")

              get (capture "/watcher/:watcher/channel/list") $ do
                requestedWatcher <- param (T.pack "watcher")
                dashboardState <- liftIO $ readIORef ioref
                json $ listWatcherChannel requestedWatcher dashboardState

              get (capture "/channel/:channel/chat/mode") $ do
                requestedChannel <- param (T.pack "channel")
                status status404 *> text (T.pack $ "Not Implemented: " ++ requestedChannel)

              get (capture "/channel/:channel/chat/list") $ do
                requestedChannel <- param (T.pack "channel")
                dashboardState <- liftIO $ readIORef ioref
                case listChat requestedChannel dashboardState of
                  Just chat -> json chat
                  Nothing -> status status404 *> text (T.pack "Channel Not Found")

              get (capture "/channel/list") $ do
                dashboardState <- liftIO $ readIORef ioref
                json $ listChannels dashboardState

        run port $ gzip def $ staticPolicy (addBase "html/") app

  void $ fork $ shutdownLoop threadId ioref

getWatchedChannel :: Channel -> ChannelName
getWatchedChannel = fst

getSpokenChannel :: Spoken -> ChannelName
getSpokenChannel = fst

getChannelName :: Channel -> ChannelName
getChannelName = fst

getWatcherName :: Watcher -> WatcherName
getWatcherName (_, watcherName, _) = watcherName

isRelevantMessage :: [MessageUniqueIdentifier] -> Message -> Bool
isRelevantMessage relevantMessages (messageUniqueIdentifier, _, _, _) = messageUniqueIdentifier `elem` relevantMessages

isWatchingChannel :: ChannelName -> Channel -> Bool
isWatchingChannel requestedChannel = (==) requestedChannel . fst

isSpeakingInChannel :: ChannelName -> Spoken -> Bool
isSpeakingInChannel requestedChannel = (==) requestedChannel . fst

isWatcherElemOf :: [WatcherUniqueIdentifier] -> Watcher -> Bool
isWatcherElemOf listOfWatchers (watcherUniqueIdentifier, _, _) = watcherUniqueIdentifier `elem` listOfWatchers

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

listWatcherChat :: ChannelName -> WatcherUniqueIdentifier -> DashboardState -> Maybe [Message]
listWatcherChat requestedChannel watcherUniqueIdentifier dashboardState =
  let thisChannel = filter (isSpeakingInChannel requestedChannel) $ speaking dashboardState
   in if not $ null thisChannel
        then
          let messageUniqueIdentifiers = snd $ head thisChannel
              relevantMessages = filter (isSpokenBy watcherUniqueIdentifier) $ filter (isRelevantMessage messageUniqueIdentifiers) $ messages dashboardState
           in Just relevantMessages
        else Nothing

isSpokenBy :: WatcherUniqueIdentifier -> Message -> Bool
isSpokenBy watcherUniqueIdentifier (_, spokenByUniqueIdentifier, _, _) = watcherUniqueIdentifier == spokenByUniqueIdentifier

listWatcherChannel :: WatcherUniqueIdentifier -> DashboardState -> [ChannelName]
listWatcherChannel watcherUniqueIdentifier dashboardState =
  let channelsTheyAreWatching = filter (isInChannel watcherUniqueIdentifier) $ watching dashboardState
   in map getChannelName channelsTheyAreWatching

isInChannel :: WatcherUniqueIdentifier -> Channel -> Bool
isInChannel watcherUniqueIdentifier (_, watchersOfChannel) = elem watcherUniqueIdentifier $ map fst watchersOfChannel

listWatcher :: ChannelName -> DashboardState -> Maybe [Watcher]
listWatcher requestedChannel dashboardState = do
  let watchersOfChannel = filter (isWatchingChannel requestedChannel) $ watching dashboardState
  if (not . null) watchersOfChannel
    then
      let watchingThisChannel = snd $ head watchersOfChannel
          whoIs = fst
          listOfWatcherUniqueIdentifier = map whoIs watchingThisChannel
          listOfWatcher = filter (isWatcherElemOf listOfWatcherUniqueIdentifier) $ watchers dashboardState
       in Just listOfWatcher
    else Nothing

listChat :: ChannelName -> DashboardState -> Maybe [Message]
listChat requestedChannel dashboardState =
  let thisChannel = filter (isSpeakingInChannel requestedChannel) $ speaking dashboardState
   in if not $ null thisChannel
        then
          let messageUniqueIdentifiers = snd $ head thisChannel
              relevantMessages = filter (isRelevantMessage messageUniqueIdentifiers) $ messages dashboardState
           in Just relevantMessages
        else Nothing

listChannels :: DashboardState -> [ChannelName]
listChannels dashboardState =
  let spokenChannels = map getSpokenChannel $ speaking dashboardState
      watchedChannels = map getWatchedChannel $ watching dashboardState
   in sortUniq $ spokenChannels ++ watchedChannels

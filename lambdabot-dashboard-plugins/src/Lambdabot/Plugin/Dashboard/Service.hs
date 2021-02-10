module Lambdabot.Plugin.Dashboard.Service (startListening) where

import Lambdabot.Plugin.Dashboard.Configuration (Channel, ChannelName, Dashboard, DashboardState, Message, MessageUniqueIdentifier, Spoken, WatcherName, messages, shutdown, speaking, watchers, watching)

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
import Network.Wai.Middleware.Cors (simpleCors)
import Web.Scotty (get, json, param, scotty, text)
import Web.Scotty.Trans (capture, middleware, status)

startListening :: Int -> Dashboard ()
startListening port = do
  debugM "Starting Server..."

  ids <- readMS
  ioref <- liftIO $ newIORef ids

  threadId <- fork $
    io $
      scotty port $ do
        middleware simpleCors
        -- only recognizes active channels
        -- produces Array<string>
        --                 nick
        get (capture "/:channel/viewer/list") $ do
          requestedChannel <- param (T.pack "channel")
          dashboardState <- liftIO $ readIORef ioref
          let viewerList = listViewers requestedChannel dashboardState
          json viewerList

        -- only recognizes active channels
        -- produces ["NormalParticipation" | "FollowerOnly" | "SubscriberOnly", "NormalSpeed" | "SlowSpeed"]
        --                                  participation                     ,       speed
        get (capture "/:channel/chat/mode") $ do
          requestedChannel <- param (T.pack "channel")
          status status404 *> text (T.pack $ "Not Implemented: " ++ requestedChannel)

        get (capture "/:channel/chat") $ do
          requestedChannel <- param (T.pack "channel")
          dashboardState <- liftIO $ readIORef ioref
          case listChat requestedChannel dashboardState of
            Just chat -> json chat
            Nothing -> status status404 *> text (T.pack "Channel Not Found")

        get (capture "/channel/list") $ do
          dashboardState <- liftIO $ readIORef ioref
          json $ listChannels dashboardState

  void $ fork $ shutdownLoop threadId ioref

getWatchedChannel :: Channel -> ChannelName
getWatchedChannel = fst

getSpokenChannel :: Spoken -> ChannelName
getSpokenChannel = fst

relevantMessage :: [MessageUniqueIdentifier] -> Message -> Bool
relevantMessage relevantMessages (messageUniqueIdentifier, _, _, _) = messageUniqueIdentifier `elem` relevantMessages

shutdownLoop :: ThreadId -> IORef DashboardState -> Dashboard ()
shutdownLoop threadId ioref = do
  threadDelay $ 500 * 1000
  withMS $ \dashboardState _ -> do
    void $ liftIO $ writeIORef ioref dashboardState
    if shutdown dashboardState
      then do
        debugM "Killing listener thread."
        killThread threadId
      else void $ fork $ shutdownLoop threadId ioref

listViewers :: ChannelName -> DashboardState -> [WatcherName]
listViewers requestedChannel dashboardState = do
  let watchersOfChannel = filter ((==) requestedChannel . fst) $ watching dashboardState
  if (not . null) watchersOfChannel
    then
      let listOfWatcher = map fst $ snd $ head watchersOfChannel
       in map (\(_, watcherName, _) -> watcherName) $ filter (\(uniqueWatcherIdentifier, _, _) -> uniqueWatcherIdentifier `elem` listOfWatcher) $ watchers dashboardState
    else []

listChat :: ChannelName -> DashboardState -> Maybe [Message]
listChat requestedChannel dashboardState =
  let thisChannel = filter ((==) requestedChannel . fst) $ speaking dashboardState
   in if not $ null thisChannel
        then
          let messageUniqueIdentifiers = snd $ head thisChannel
              relevantMessages = filter (relevantMessage messageUniqueIdentifiers) $ messages dashboardState
           in Just relevantMessages
        else Nothing

listChannels :: DashboardState -> [ChannelName]
listChannels dashboardState =
  let spokenChannels = map getSpokenChannel $ speaking dashboardState
      watchedChannels = map getWatchedChannel $ watching dashboardState
   in sortUniq $ spokenChannels ++ watchedChannels

{-# LANGUAGE FlexibleInstances #-}

module Lambdabot.Plugin.Dashboard.Dashboard where

import Lambdabot.Config.Dashboard (dashboardPort)
import Lambdabot.Plugin.Dashboard.Configuration

import Lambdabot.IRC (IrcMessage, ircMsgParams)
import Lambdabot.Logging (debugM, noticeM)
import qualified Lambdabot.Message as Msg
import Lambdabot.Monad (registerCallback)
import Lambdabot.Plugin (LB, Module, ModuleT, MonadLBState (withMS), Nick (..), getConfig, moduleDefState, moduleExit, moduleInit, moduleSerialize, newModule, readMS, stdSerial)
import Lambdabot.Util (io)

import Control.Concurrent.Lifted (ThreadId, fork, killThread, threadDelay)
import Control.Monad.Reader (liftIO)
import Data.Functor (void)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (isInfixOf, partition)
import Data.Maybe (fromJust)
import qualified Data.Text.Lazy as T
import Network.HTTP.Types.Status (status404)
import Network.Wai.Middleware.Cors (simpleCors)
import Web.Scotty (get, json, param, scotty, text)
import Web.Scotty.Trans (capture, middleware, status)

type Dashboard = ModuleT DashboardState LB

data MessageType = Join | Part | Quit | Rename | Speak | Room | User | Audience deriving (Eq)

dashboardPlugin :: Module DashboardState
dashboardPlugin =
  newModule
    { moduleSerialize = Just stdSerial
    , moduleDefState = do
        pure $ MkDashboardState{focusedChannel = "", shutdown = False, watchers = [], watching = [], messages = [], speaking = []}
    , moduleInit = do
        withMS $ \dashboardState writer -> writer dashboardState{shutdown = False}
        sequence_
          [ registerCallback signal (withDashboardFM cb)
          | (signal, cb) <-
              zip
                ["JOIN", "PART", "QUIT", "NICK", "353", "PRIVMSG", "USERSTATE", "ROOMSTATE"]
                [ updateState Join
                , updateState Part
                , updateState Quit
                , updateState Rename
                , updateState Audience
                , updateState Speak
                , updateState User
                , updateState Room
                ]
          ]
        port <- getConfig dashboardPort
        startListening port
    , moduleExit = withMS $ \dashboardState writer -> writer dashboardState{shutdown = True}
    }

withDashboardFM :: Msg.Message a => (a -> Nick -> Nick -> DashboardState -> Maybe DashboardState) -> (a -> Dashboard ())
withDashboardFM handler msg = do
  debugM "executing withDashboardFM"
  let channel = head . Msg.channels $! msg
  let nickname = Msg.nick msg
  noticeM $ "Received :: " ++ nTag channel ++ " <> " ++ nName channel ++ " <> " ++ nTag nickname ++ " <> " ++ nName nickname
  withMS $ \dashboardState writer -> case handler msg channel nickname dashboardState of
    Just updatedState -> writer updatedState
    Nothing -> pure ()

updateState :: MessageType -> IrcMessage -> Nick -> Nick -> DashboardState -> Maybe DashboardState
updateState notice msg chnl sndr dashboardState =
  let uniqueWatcherIdentifier = nName sndr
      channelName = nName chnl
      watcherName = nName sndr
      wtchr = (uniqueWatcherIdentifier, watcherName, Nothing)
      newWatching = (uniqueWatcherIdentifier, [])
      maybeMessage = if notice == Speak then Just ("100-200", watcherName, unwords $ ircMsgParams msg, []) else Nothing
   in updateState' notice channelName wtchr newWatching maybeMessage dashboardState

updateState' :: MessageType -> ChannelName -> Watcher -> Watching -> Maybe Message -> DashboardState -> Maybe DashboardState
updateState' messageType channelName watcher newWatching maybeMessage dashboardState =
  let partitionedWatching = partition ((==) channelName . fst) $ watching dashboardState
      partitionedWatchers = partition (`sameWatcher` watcher) (watchers dashboardState)
      partitionedSpeaking = partition ((==) channelName . fst) $ speaking dashboardState
      todo = case messageType of
        Join -> Just [addWatcher partitionedWatchers watcher, addWatching partitionedWatching channelName newWatching]
        Speak -> Just [addMessage partitionedSpeaking channelName (fromJust maybeMessage), addWatcher partitionedWatchers watcher, addWatching partitionedWatching channelName newWatching]
        Part -> Just [addWatcher partitionedWatchers watcher, removeWatching partitionedWatching channelName newWatching]
        _ -> Nothing
   in updateState'' todo dashboardState

updateState'' :: Maybe [Either DashboardState DashboardState -> Either DashboardState DashboardState] -> DashboardState -> Maybe DashboardState
updateState'' todo dashboardState = case todo of
  Just fs -> case foldl (\c f -> f c) (Left dashboardState) fs of
    Right updatedState -> Just updatedState
    Left _ -> Nothing
  Nothing -> Nothing

sameWatcher :: Watcher -> Watcher -> Bool
sameWatcher (lwuid, _, _) (rwuid, _, _) = lwuid == rwuid

sameWatching :: Watching -> Watching -> Bool
sameWatching (lwn, _) (rwn, _) = lwn == rwn

addMessage :: ([Spoken], [Spoken]) -> ChannelName -> Message -> Either DashboardState DashboardState -> Either DashboardState DashboardState
addMessage (channelSpoken, otherSpoken) channelName theMessage@(uniqueMessageIdentifier, _, _, _) (Left dashboardState) =
  let umids = if null channelSpoken then [] else snd $ head channelSpoken
   in Right
        dashboardState
          { messages = theMessage : messages dashboardState
          , speaking = (channelName, uniqueMessageIdentifier : umids) : otherSpoken
          }
addMessage (channelSpoken, otherSpoken) channelName theMessage@(uniqueMessageIdentifier, _, _, _) (Right dashboardState) =
  let umids = if null channelSpoken then [] else snd $ head channelSpoken
   in Right
        dashboardState
          { messages = theMessage : messages dashboardState
          , speaking = (channelName, uniqueMessageIdentifier : umids) : otherSpoken
          }

addWatcher :: ([Watcher], [Watcher]) -> Watcher -> Either DashboardState DashboardState -> Either DashboardState DashboardState
addWatcher (thisWatcher, otherWatcher) newWatcher (Left dashboardState) =
  if null thisWatcher
    then Right dashboardState{watchers = newWatcher : otherWatcher}
    else
      let existingWatcher = head thisWatcher
       in if existingWatcher `sameWatcher` newWatcher
            then Left dashboardState
            else Right dashboardState{watchers = newWatcher : otherWatcher}
addWatcher (thisWatcher, otherWatcher) newWatcher (Right dashboardState) =
  if null thisWatcher
    then Right dashboardState{watchers = newWatcher : otherWatcher}
    else
      let existingWatcher = head thisWatcher
       in if existingWatcher `sameWatcher` newWatcher
            then Right dashboardState
            else Right dashboardState{watchers = newWatcher : otherWatcher}

addWatching :: ([Channel], [Channel]) -> ChannelName -> Watching -> Either DashboardState DashboardState -> Either DashboardState DashboardState
addWatching (thisChannel, otherChannel) channelName newWatching (Left dashboardState) =
  if null thisChannel
    then Right dashboardState{watching = (channelName, [newWatching]) : otherChannel}
    else
      let (thisWatching, otherWatching) = partition (`sameWatching` newWatching) $ snd $ head thisChannel
       in if null thisWatching
            then Right dashboardState{watching = (channelName, newWatching : otherWatching) : otherChannel}
            else Left dashboardState
addWatching (thisChannel, otherChannel) channelName newWatching (Right dashboardState) =
  if null thisChannel
    then Right dashboardState{watching = (channelName, [newWatching]) : otherChannel}
    else
      let (thisWatching, otherWatching) = partition (`sameWatching` newWatching) $ snd $ head thisChannel
       in if null thisWatching
            then Right dashboardState{watching = (channelName, newWatching : otherWatching) : otherChannel}
            else Right dashboardState

removeWatching :: ([Channel], [Channel]) -> ChannelName -> Watching -> Either DashboardState DashboardState -> Either DashboardState DashboardState
removeWatching (thisChannel, otherChannel) channelName (uniqueWatcherIdentifier, _) (Left dashboardState) =
  if not $ null thisChannel
    then
      let (thisWatching, otherWatching) = partition ((==) uniqueWatcherIdentifier . fst) $ snd $ head thisChannel
       in if not $ null thisWatching
            then Right dashboardState{watching = (channelName, otherWatching) : otherChannel}
            else Left dashboardState
    else Left dashboardState
removeWatching (thisChannel, otherChannel) channelName (uniqueWatcherIdentifier, _) (Right dashboardState) =
  if not $ null thisChannel
    then
      let (thisWatching, otherWatching) = partition ((==) uniqueWatcherIdentifier . fst) $ snd $ head thisChannel
       in if not $ null thisWatching
            then Right dashboardState{watching = (channelName, otherWatching) : otherChannel}
            else Right dashboardState
    else Right dashboardState

tagNamed :: Eq a => a -> [(a, b)] -> [(a, b)]
tagNamed named = filter ((==) named . fst)

isTruthy :: [(a, String)] -> Bool
isTruthy = (==) "1" . (snd . head)

whenNotNull :: ([a] -> Bool) -> [a] -> Bool
whenNotNull _ [] = False
whenNotNull f ls = f ls

tagValueContains :: String -> [(a, String)] -> Bool
tagValueContains named = isInfixOf named . (snd . head)

listViewers :: ChannelName -> DashboardState -> [WatcherName]
listViewers requestedChannel dashboardState = do
  let watchersOfChannel = filter ((==) requestedChannel . fst) $ watching dashboardState
  if (not . null) watchersOfChannel
    then
      let listOfWatcher = map fst $ snd $ head watchersOfChannel
       in map (\(_, watcherName, _) -> watcherName) $ filter (\(uniqueWatcherIdentifier, _, _) -> uniqueWatcherIdentifier `elem` listOfWatcher) $ watchers dashboardState
    else []

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
        -- produces Array<[string, boolean, boolean]>
        --                 nick  , sub    , mod
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
        -- only recognizes active channels
        -- produces "Online" | "Offline"
        get (capture "/:channel/state") $ do
          requestedChannel <- param (T.pack "channel")
          status status404 *> text (T.pack $ "Not Implemented: " ++ requestedChannel)
        -- produces Array<[string, boolean]>
        get (capture "/channel/list") $ do
          json channelList
  void $ fork $ shutdownLoop threadId ioref

channelList :: [(String, Bool)]
channelList =
  [ ("#swarmcollective", True)
  , ("#android272", True)
  ]

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

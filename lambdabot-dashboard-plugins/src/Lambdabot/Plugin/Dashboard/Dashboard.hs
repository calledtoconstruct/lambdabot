{-# LANGUAGE FlexibleInstances #-}

module Lambdabot.Plugin.Dashboard.Dashboard where

import Lambdabot.Config.Dashboard (dashboardPort, garbageCollectionIntervalInSeconds)
import Lambdabot.Plugin.Dashboard.Configuration (
  Channel,
  ChannelName,
  Dashboard,
  DashboardState (..),
  Message,
  Spoken,
  Watcher,
  WatcherName,
  Watching,
 )
import Lambdabot.Plugin.Dashboard.Garbage (startCollectingGarbage)
import Lambdabot.Plugin.Dashboard.Service (startListening)

import Lambdabot.IRC (IrcMessage, ircMsgParams, ircTags)
import Lambdabot.Logging (noticeM)
import qualified Lambdabot.Message as Msg
import Lambdabot.Monad (registerCallback)
import Lambdabot.Plugin (Module, MonadLBState (withMS), Nick (..), getConfig, moduleDefState, moduleExit, moduleInit, moduleSerialize, newModule, stdSerial)

import Data.List (partition)
import Data.Maybe (fromJust, fromMaybe)

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
        interval <- getConfig garbageCollectionIntervalInSeconds
        port <- getConfig dashboardPort
        startListening port
        startCollectingGarbage interval
    , moduleExit = withMS $ \dashboardState writer -> writer dashboardState{shutdown = True}
    }

withDashboardFM :: Msg.Message a => (a -> Nick -> Nick -> DashboardState -> Maybe DashboardState) -> (a -> Dashboard ())
withDashboardFM handler msg = do
  let channel = head . Msg.channels $! msg
      nickname = Msg.nick msg
  noticeM $ "Received :: " ++ nTag channel ++ " <> " ++ nName channel ++ " <> " ++ nTag nickname ++ " <> " ++ nName nickname
  withMS $ \dashboardState writer -> case handler msg channel nickname dashboardState of
    Just updatedState -> writer updatedState
    Nothing -> pure ()

updateState :: MessageType -> IrcMessage -> Nick -> Nick -> DashboardState -> Maybe DashboardState
updateState User _ chnl _ dashboardState =
  let channelName = nName chnl
      (_, otherChannel) = partition ((==) channelName . fst) $ watching dashboardState
   in Just dashboardState{watching = (channelName, []) : otherChannel}
updateState Audience msg chnl sndr dashboardState =
  let sansBotName = tail $ tail $ ircMsgParams msg
      channelName = head sansBotName
      listOfWatcher = words $ tail $ head $ tail sansBotName
   in Just $ foldl (audienceJoin msg chnl{nName = channelName} sndr) dashboardState listOfWatcher
updateState messageType msg chnl sndr dashboardState =
  let uniqueWatcherIdentifier = nName sndr
      channelName = nName chnl
      watcherName = nName sndr
      wtchr = (uniqueWatcherIdentifier, watcherName, Nothing)
      newWatching = (uniqueWatcherIdentifier, [])
   in updateState' messageType msg channelName wtchr newWatching dashboardState

audienceJoin :: IrcMessage -> Nick -> Nick -> DashboardState -> WatcherName -> DashboardState
audienceJoin msg chnl sndr dashboardState watcher = fromMaybe dashboardState (updateState Join msg chnl (Nick{nName = watcher, nTag = nTag sndr}) dashboardState)

updateState' :: MessageType -> IrcMessage -> ChannelName -> Watcher -> Watching -> DashboardState -> Maybe DashboardState
updateState' Speak msg channelName watcher newWatching@(uniqueWatcherIdentifier, _) dashboardState =
  if null $ ircTags msg
    then Nothing
    else
      let idTags = tagNamed "id" $ ircTags msg
       in if null idTags
            then Nothing
            else
              let uniqueMessageIdentifier = snd $ head idTags
                  maybeMessage = Just (uniqueMessageIdentifier, uniqueWatcherIdentifier, unwords $ tail $ ircMsgParams msg, [])
               in updateState'' Speak channelName watcher newWatching maybeMessage dashboardState
updateState' messageType _ channelName watcher newWatching dashboardState = updateState'' messageType channelName watcher newWatching Nothing dashboardState

updateState'' :: MessageType -> ChannelName -> Watcher -> Watching -> Maybe Message -> DashboardState -> Maybe DashboardState
updateState'' messageType channelName watcher newWatching maybeMessage dashboardState =
  let partitionedWatching = partition ((==) channelName . fst) $ watching dashboardState
      partitionedWatchers = partition (`sameWatcher` watcher) (watchers dashboardState)
      partitionedSpeaking = partition ((==) channelName . fst) $ speaking dashboardState
      addWatcher' = addWatcher partitionedWatchers watcher
      addWatching' = addWatching partitionedWatching channelName newWatching
      addMessage' = addMessage partitionedSpeaking channelName (fromJust maybeMessage)
      removeWatching' = removeWatching partitionedWatching channelName newWatching
      todo = case messageType of
        Join -> Just [addWatcher', addWatching']
        Speak -> Just [addMessage', addWatcher', addWatching']
        Part -> Just [addWatcher', removeWatching']
        _ -> Nothing
   in updateState''' todo dashboardState

updateState''' :: Maybe [Either DashboardState DashboardState -> Either DashboardState DashboardState] -> DashboardState -> Maybe DashboardState
updateState''' todo dashboardState = case todo of
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

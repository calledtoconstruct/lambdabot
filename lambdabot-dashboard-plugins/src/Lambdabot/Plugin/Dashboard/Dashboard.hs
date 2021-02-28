{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Lambdabot.Plugin.Dashboard.Dashboard where

import Lambdabot.Config.Dashboard (dashboardPort, garbageCollectionIntervalInSeconds)
import Lambdabot.Plugin.Dashboard.Configuration (
  Badge (MkBadge),
  Channel (..),
  ChannelName,
  Dashboard,
  DashboardState (..),
  Message (..),
  Spoken (..),
  Watcher (..),
  WatcherName,
  Watching (..),
  channelNameFromSpoken,
  listOfMessageUniqueIdentifierFromSpoken,
  nameFromChannel,
  uniqueIdentifierFromWatcher,
  uniqueIdentifierFromWatching,
  watchingFromChannel,
 )
import Lambdabot.Plugin.Dashboard.Garbage (startCollectingGarbage)
import Lambdabot.Plugin.Dashboard.Service (startListening)

import Lambdabot.IRC (IrcMessage, IrcTag, ircMsgParams, ircTags)
import Lambdabot.Logging (noticeM)
import qualified Lambdabot.Message as Msg
import Lambdabot.Monad (registerCallback)
import Lambdabot.Plugin (Module, MonadLBState (withMS), Nick (..), getConfig, moduleDefState, moduleExit, moduleInit, moduleSerialize, newModule, stdSerial)
import Lambdabot.Plugin.Dashboard.StateChange (StateChange (..), fromStateChange, whenModified)

import Data.List (partition)
import Data.Maybe (fromJust)
import qualified Data.Text as T

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
              [ ("JOIN", updateState Join)
              , ("PART", updateState Part)
              , ("QUIT", updateState Quit)
              , ("NICK", updateState Rename)
              , ("353", updateState Audience)
              , ("PRIVMSG", updateState Speak)
              , ("USERSTATE", updateState User)
              , ("ROOMSTATE", updateState Room)
              ]
          ]
        getConfig dashboardPort >>= startListening
        getConfig garbageCollectionIntervalInSeconds >>= startCollectingGarbage
    , moduleExit = withMS $ \dashboardState writer -> writer dashboardState{shutdown = True}
    }

withDashboardFM :: Msg.Message a => (a -> Nick -> Nick -> StateChange DashboardState -> StateChange DashboardState) -> (a -> Dashboard ())
withDashboardFM handler msg = do
  let channel = head . Msg.channels $! msg
      nickname = Msg.nick msg
  noticeM $ "Received :: " ++ nTag channel ++ " <> " ++ nName channel ++ " <> " ++ nTag nickname ++ " <> " ++ nName nickname
  withMS $ \dashboardState writer -> whenModified (pure ()) writer $ handler msg channel nickname (Original dashboardState)

updateState :: MessageType -> IrcMessage -> Nick -> Nick -> StateChange DashboardState -> StateChange DashboardState
updateState User _ chnl _ stateChange =
  let dashboardState = fromStateChange stateChange
      channelName = T.pack $ nName chnl
      otherChannel = filter ((channelName /=) . nameFromChannel) $ watching dashboardState
   in Modified dashboardState{watching = MkChannel (channelName, []) : otherChannel}
updateState Audience msg chnl sndr stateChange =
  let sansBotName = map T.pack $ tail $ tail $ ircMsgParams msg
      channelName = head sansBotName
      listOfWatcher = T.words $ T.tail $ head $ tail sansBotName
   in foldl (audienceJoin msg chnl{nName = T.unpack channelName} sndr) stateChange listOfWatcher
updateState messageType msg chnl sndr stateChange =
  let uniqueWatcherIdentifier = T.pack $ nName sndr
      channelName = T.pack $ nName chnl
      watcherName = T.pack $ nName sndr
      wtchr = MkWatcher (uniqueWatcherIdentifier, watcherName, Nothing)
      newWatching = MkWatching (uniqueWatcherIdentifier, [])
   in updateState' messageType msg channelName wtchr newWatching stateChange

audienceJoin :: IrcMessage -> Nick -> Nick -> StateChange DashboardState -> WatcherName -> StateChange DashboardState
audienceJoin msg chnl sndr stateChange watcher = updateState Join msg chnl (Nick{nName = T.unpack watcher, nTag = nTag sndr}) stateChange

updateState' :: MessageType -> IrcMessage -> ChannelName -> Watcher -> Watching -> StateChange DashboardState -> StateChange DashboardState
updateState' Speak msg channelName watcher newWatching@(MkWatching (uniqueWatcherIdentifier, _)) stateChange =
  if null $ ircTags msg
    then stateChange
    else
      let idTags = tagNamed "id" $ ircTags msg
          updatedWatcher = maybeUpdateWatcherName watcher $ tagNamed "display-name" $ ircTags msg
          updatedWatching = maybeAddTagToWatchingBadges newWatching $ tagNamed "mod" $ ircTags msg
       in if null idTags
            then stateChange
            else
              let uniqueMessageIdentifier = T.pack $ snd $ head idTags
                  maybeMessage = Just $ MkMessage (uniqueMessageIdentifier, uniqueWatcherIdentifier, getTextOfMessage msg, [])
               in updateState'' Speak channelName updatedWatcher updatedWatching maybeMessage stateChange
updateState' messageType _ channelName watcher newWatching stateChange = updateState'' messageType channelName watcher newWatching Nothing stateChange

maybeAddTagToWatchingBadges :: Watching -> [IrcTag] -> Watching
maybeAddTagToWatchingBadges existingWatching@(MkWatching (uniqueWatcherIdentifier, badges)) modTag =
  if not $ null modTag
    then
      let (tagName, tagValue) = head modTag
       in MkWatching (uniqueWatcherIdentifier, MkBadge (T.pack tagName, read tagValue) : badges)
    else existingWatching

-- Skip first param (channel name)
-- Trim colon prefix
getTextOfMessage :: IrcMessage -> T.Text
getTextOfMessage = T.pack . tail . unwords . drop 1 . ircMsgParams

maybeUpdateWatcherName :: Watcher -> [IrcTag] -> Watcher
maybeUpdateWatcherName watcher@(MkWatcher (uniqueWatcherIdentifier, _, watcherSystemIdentifier)) displayNameTag =
  if not $ null displayNameTag
    then MkWatcher (uniqueWatcherIdentifier, (T.pack . snd . head) displayNameTag, watcherSystemIdentifier)
    else watcher

updateState'' :: MessageType -> ChannelName -> Watcher -> Watching -> Maybe Message -> StateChange DashboardState -> StateChange DashboardState
updateState'' messageType channelName watcher newWatching maybeMessage stateChange =
  let dashboardState = fromStateChange stateChange
      partitionedWatching = partition ((channelName ==) . nameFromChannel) $ watching dashboardState
      partitionedWatchers = partition (`sameWatcher` watcher) (watchers dashboardState)
      partitionedSpeaking = partition ((channelName ==) . channelNameFromSpoken) $ speaking dashboardState
      addOrUpdateWatcher' = addOrUpdateWatcher partitionedWatchers watcher
      addOrUpdateWatching' = addOrUpdateWatching partitionedWatching channelName newWatching
      addMessage' = addMessage partitionedSpeaking channelName (fromJust maybeMessage)
      removeWatching' = removeWatching partitionedWatching channelName newWatching
      todo = case messageType of
        Join -> Just [addOrUpdateWatcher', addOrUpdateWatching']
        Speak -> Just [addMessage', addOrUpdateWatcher', addOrUpdateWatching']
        Part -> Just [addOrUpdateWatcher', removeWatching']
        _ -> Nothing
   in updateState''' todo stateChange

updateState''' :: Maybe [StateChange DashboardState -> StateChange DashboardState] -> StateChange DashboardState -> StateChange DashboardState
updateState''' todo stateChange = case todo of
  Just strategies -> foldl (\state strategy -> strategy state) stateChange strategies
  Nothing -> stateChange

sameWatcher :: Watcher -> Watcher -> Bool
sameWatcher a b = uniqueIdentifierFromWatcher a == uniqueIdentifierFromWatcher b

sameWatching :: Watching -> Watching -> Bool
sameWatching a b = uniqueIdentifierFromWatching a == uniqueIdentifierFromWatching b

addMessage :: ([Spoken], [Spoken]) -> ChannelName -> Message -> StateChange DashboardState -> StateChange DashboardState
addMessage (channelSpoken, otherSpoken) channelName theMessage@(MkMessage (uniqueMessageIdentifier, _, _, _)) (Original dashboardState) =
  let umids = if null channelSpoken then [] else listOfMessageUniqueIdentifierFromSpoken $ head channelSpoken
   in Modified
        dashboardState
          { messages = theMessage : messages dashboardState
          , speaking = MkSpoken (channelName, uniqueMessageIdentifier : umids) : otherSpoken
          }
addMessage (channelSpoken, otherSpoken) channelName theMessage@(MkMessage (uniqueMessageIdentifier, _, _, _)) (Modified dashboardState) =
  let umids = if null channelSpoken then [] else listOfMessageUniqueIdentifierFromSpoken $ head channelSpoken
   in Modified
        dashboardState
          { messages = theMessage : messages dashboardState
          , speaking = MkSpoken (channelName, uniqueMessageIdentifier : umids) : otherSpoken
          }

addOrUpdateWatcher :: ([Watcher], [Watcher]) -> Watcher -> StateChange DashboardState -> StateChange DashboardState
addOrUpdateWatcher (thisWatcher, otherWatcher) newWatcher (Original dashboardState) =
  if null thisWatcher
    then Modified dashboardState{watchers = newWatcher : otherWatcher}
    else
      let existingWatcher = head thisWatcher
       in if existingWatcher == newWatcher
            then Original dashboardState
            else Modified dashboardState{watchers = newWatcher : otherWatcher}
addOrUpdateWatcher (thisWatcher, otherWatcher) newWatcher (Modified dashboardState) =
  if null thisWatcher
    then Modified dashboardState{watchers = newWatcher : otherWatcher}
    else
      let existingWatcher = head thisWatcher
       in if existingWatcher == newWatcher
            then Modified dashboardState
            else Modified dashboardState{watchers = newWatcher : otherWatcher}

addOrUpdateWatching :: ([Channel], [Channel]) -> ChannelName -> Watching -> StateChange DashboardState -> StateChange DashboardState
addOrUpdateWatching (thisChannel, otherChannel) channelName newWatching (Original dashboardState) =
  if null thisChannel
    then Modified dashboardState{watching = MkChannel (channelName, [newWatching]) : otherChannel}
    else
      let (thisWatching, otherWatching) = partition (`sameWatching` newWatching) $ watchingFromChannel $ head thisChannel
       in if null thisWatching || head thisWatching /= newWatching
            then Modified dashboardState{watching = MkChannel (channelName, newWatching : otherWatching) : otherChannel}
            else Original dashboardState
addOrUpdateWatching (thisChannel, otherChannel) channelName newWatching (Modified dashboardState) =
  if null thisChannel
    then Modified dashboardState{watching = MkChannel (channelName, [newWatching]) : otherChannel}
    else
      let (thisWatching, otherWatching) = partition (`sameWatching` newWatching) $ watchingFromChannel $ head thisChannel
       in if null thisWatching || head thisWatching /= newWatching
            then Modified dashboardState{watching = MkChannel (channelName, newWatching : otherWatching) : otherChannel}
            else Modified dashboardState

removeWatching :: ([Channel], [Channel]) -> ChannelName -> Watching -> StateChange DashboardState -> StateChange DashboardState
removeWatching (thisChannel, otherChannel) channelName (MkWatching (uniqueWatcherIdentifier, _)) (Original dashboardState) =
  if not $ null thisChannel
    then
      let (thisWatching, otherWatching) = partition ((==) uniqueWatcherIdentifier . uniqueIdentifierFromWatching) $ watchingFromChannel $ head thisChannel
       in if not $ null thisWatching
            then Modified dashboardState{watching = MkChannel (channelName, otherWatching) : otherChannel}
            else Original dashboardState
    else Original dashboardState
removeWatching (thisChannel, otherChannel) channelName (MkWatching (uniqueWatcherIdentifier, _)) (Modified dashboardState) =
  if not $ null thisChannel
    then
      let (thisWatching, otherWatching) = partition ((==) uniqueWatcherIdentifier . uniqueIdentifierFromWatching) $ watchingFromChannel $ head thisChannel
       in if not $ null thisWatching
            then Modified dashboardState{watching = MkChannel (channelName, otherWatching) : otherChannel}
            else Modified dashboardState
    else Modified dashboardState

tagNamed :: Eq a => a -> [(a, b)] -> [(a, b)]
tagNamed named = filter ((==) named . fst)

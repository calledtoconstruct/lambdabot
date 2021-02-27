{-# LANGUAGE DeriveGeneric #-}

module Lambdabot.Plugin.Dashboard.Configuration (
  DashboardState (..),
  Dashboard,
  WatcherUniqueIdentifier,
  WatcherSystemIdentifier,
  MessageUniqueIdentifier,
  ChannelName,
  WatcherName,
  EmoteUniqueIdentifier,
  EmoteStartPosition,
  EmoteEndPosition,
  MessageText,
  BadgeName,
  BadgeVersion,
  Watcher (..),
  EmotePosition (..),
  Message (..),
  Badge (..),
  Watching (..),
  Channel (..),
  Spoken (..),
  ChannelWatcher (..),
  nameFromChannel,
  watchingFromChannel,
  uniqueIdentifierFromWatcher,
  systemIdentifierFromWatcher,
  nameFromWatcher,
  versionFromBadge,
  nameFromBadge,
  uniqueIdentifierFromWatching,
  listOfBadgesFromWatching,
  uniqueIdentifierFromEmotePosition,
  startPositionFromEmotePosition,
  endPositionFromEmotePosition,
  emotePositionsFromMessage,
  messageTextFromMessage,
  watcherUniqueIdentifierFromMessage,
  uniqueIdentifierFromMessage,
  listOfMessageUniqueIdentifierFromSpoken,
  channelNameFromSpoken,
) where

import Lambdabot.Plugin (LB, ModuleT)

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

-- Currently Twitch centric

type WatcherSystemIdentifier = Maybe Int
type WatcherUniqueIdentifier = String
type MessageUniqueIdentifier = String
type ChannelName = String
type WatcherName = String
type EmoteUniqueIdentifier = Int
type EmoteStartPosition = Int
type EmoteEndPosition = Int
type MessageText = String
type BadgeName = String
type BadgeVersion = Int

newtype Watcher = MkWatcher (WatcherUniqueIdentifier, WatcherName, WatcherSystemIdentifier) deriving (Read, Show, Eq, Generic)
uniqueIdentifierFromWatcher :: Watcher -> WatcherUniqueIdentifier
uniqueIdentifierFromWatcher (MkWatcher (ui, _, _)) = ui
nameFromWatcher :: Watcher -> WatcherName
nameFromWatcher (MkWatcher (_, nm, _)) = nm
systemIdentifierFromWatcher :: Watcher -> WatcherSystemIdentifier
systemIdentifierFromWatcher (MkWatcher (_, _, si)) = si

instance ToJSON Watcher
instance FromJSON Watcher

newtype Badge = MkBadge (BadgeName, BadgeVersion) deriving (Read, Show, Eq, Generic)
nameFromBadge :: Badge -> BadgeName
nameFromBadge (MkBadge (bn, _)) = bn
versionFromBadge :: Badge -> BadgeVersion
versionFromBadge (MkBadge (_, ver)) = ver

instance ToJSON Badge
instance FromJSON Badge

newtype Watching = MkWatching (WatcherUniqueIdentifier, [Badge]) deriving (Read, Show, Eq)
uniqueIdentifierFromWatching :: Watching -> WatcherUniqueIdentifier
uniqueIdentifierFromWatching (MkWatching (ui, _)) = ui
listOfBadgesFromWatching :: Watching -> [Badge]
listOfBadgesFromWatching (MkWatching (_, bs)) = bs

newtype Channel = MkChannel (ChannelName, [Watching]) deriving (Read, Show)
nameFromChannel :: Channel -> ChannelName
nameFromChannel (MkChannel (cn, _)) = cn
watchingFromChannel :: Channel -> [Watching]
watchingFromChannel (MkChannel (_, ws)) = ws

newtype ChannelWatcher = MkChannelWatcher (WatcherUniqueIdentifier, WatcherName, [Badge]) deriving (Generic)

instance ToJSON ChannelWatcher
instance FromJSON ChannelWatcher

newtype EmotePosition = MkEmotePosition (EmoteUniqueIdentifier, EmoteStartPosition, EmoteEndPosition) deriving (Read, Show, Generic)
uniqueIdentifierFromEmotePosition :: EmotePosition -> EmoteUniqueIdentifier
uniqueIdentifierFromEmotePosition (MkEmotePosition (ui, _, _)) = ui
startPositionFromEmotePosition :: EmotePosition -> EmoteStartPosition
startPositionFromEmotePosition (MkEmotePosition (_, sp, _)) = sp
endPositionFromEmotePosition :: EmotePosition -> EmoteEndPosition
endPositionFromEmotePosition (MkEmotePosition (_, _, ep)) = ep

instance ToJSON EmotePosition
instance FromJSON EmotePosition

newtype Message = MkMessage (MessageUniqueIdentifier, WatcherUniqueIdentifier, MessageText, [EmotePosition]) deriving (Read, Show, Generic)
uniqueIdentifierFromMessage :: Message -> MessageUniqueIdentifier
uniqueIdentifierFromMessage (MkMessage (ui, _, _, _)) = ui
watcherUniqueIdentifierFromMessage :: Message -> MessageUniqueIdentifier
watcherUniqueIdentifierFromMessage (MkMessage (ui, _, _, _)) = ui
messageTextFromMessage :: Message -> MessageUniqueIdentifier
messageTextFromMessage (MkMessage (ui, _, _, _)) = ui
emotePositionsFromMessage :: Message -> MessageUniqueIdentifier
emotePositionsFromMessage (MkMessage (ui, _, _, _)) = ui

instance ToJSON Message
instance FromJSON Message

newtype Spoken = MkSpoken (ChannelName, [MessageUniqueIdentifier]) deriving (Read, Show)
channelNameFromSpoken :: Spoken -> ChannelName
channelNameFromSpoken (MkSpoken (cn, _)) = cn
listOfMessageUniqueIdentifierFromSpoken :: Spoken -> [MessageUniqueIdentifier]
listOfMessageUniqueIdentifierFromSpoken (MkSpoken (_, muis)) = muis

data DashboardState = MkDashboardState
  { focusedChannel :: ChannelName
  , shutdown :: Bool
  , watchers :: [Watcher]
  , watching :: [Channel]
  , messages :: [Message]
  , speaking :: [Spoken]
  }
  deriving (Read, Show)

type Dashboard = ModuleT DashboardState LB
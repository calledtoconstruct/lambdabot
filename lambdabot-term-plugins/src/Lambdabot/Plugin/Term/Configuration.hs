module Lambdabot.Plugin.Term.Configuration (
  TermState (MkTermState, channels),
  TermDefinition,
  Channel (MkChannel, channelName, terms, lockedTerms, lastUnlockedTerm),
  messageTermAdded,
  messageTermRemoved,
  newTermState,
  addChannel,
  ChannelName,
  TermName,
  TermDescription,
  Message,
) where

import Lambdabot.Plugin (io)

import Control.Monad.Trans (MonadIO)
import Data.Time.Clock.System (SystemTime (systemSeconds), getSystemTime)
import GHC.Int (Int64)

type ChannelName = String
type TermDescription = String
type TermName = String
type Message = String

type TermDefinition = ([TermName], TermDescription)

data Channel = MkChannel
  { channelName :: ChannelName
  , terms :: [TermDefinition]
  , lockedTerms :: [TermName]
  , lastUnlockedTerm :: Int64
  , messageTermAdded :: Message
  , messageTermRemoved :: Message
  }
  deriving (Show, Read)

data TermState = MkTermState
  { channels :: [Channel]
  , globalDefaults :: [TermDefinition]
  }
  deriving (Show, Read)

newTermState :: [TermDefinition] -> TermState
newTermState allTheTerms =
  MkTermState
    { channels = []
    , globalDefaults = allTheTerms
    }

addChannel :: MonadIO m => TermState -> ChannelName -> m (TermState, Channel)
addChannel termState nameOfNewChannel =
  let otherChannels = filter ((/= nameOfNewChannel) . channelName) $ channels termState
   in do
        time <- io getSystemTime
        let newChannel = buildChannel termState nameOfNewChannel time
        pure
          ( termState
              { channels =
                  newChannel :
                  otherChannels
              }
          , newChannel
          )

buildChannel :: TermState -> ChannelName -> SystemTime -> Channel
buildChannel termState nameOfNewChannel time =
  MkChannel
    { channelName = nameOfNewChannel
    , terms = globalDefaults termState
    , lockedTerms = []
    , lastUnlockedTerm = systemSeconds time
    , messageTermAdded = "Term added"
    , messageTermRemoved = "Term removed"
    }
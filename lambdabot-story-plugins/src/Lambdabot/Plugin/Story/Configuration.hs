module Lambdabot.Plugin.Story.Configuration (
  StoryState (MkStoryState, games, stories, messageStoryAdded, messageStoryRemoved),
  GameState (MkGameState, server, channel, name, botName, story, votes),
  Definition,
  Vote,
  newStoryState,
  newGameState,
) where

import Lambdabot.Plugin (Nick, nName, nTag)

import Control.Monad.Trans (MonadIO)

type Definition = (String, String)
type Vote = (String, Int)

data GameState = MkGameState
  { server :: String
  , channel :: String
  , name :: String
  , botName :: String
  , story :: Definition
  , votes :: [Vote]
  }
  deriving (Show, Read)

data StoryState = MkStoryState
  { games :: [(String, GameState)]
  , stories :: [Definition]
  , messageStoryAdded :: String
  , messageStoryRemoved :: String
  }
  deriving (Show, Read)

newStoryState :: MonadIO m => [Definition] -> m StoryState
newStoryState allTheStories = do
  pure
    MkStoryState
      { games = []
      , stories = allTheStories
      , messageStoryAdded = "Story added"
      , messageStoryRemoved = "Story removed"
      }

newGameState :: String -> Nick -> Nick -> Definition -> GameState
newGameState srvr nick botn chosenStory =
  MkGameState
    { server = srvr
    , channel = nName nick
    , name = nTag nick
    , botName = nName botn
    , story = chosenStory
    , votes = []
    }
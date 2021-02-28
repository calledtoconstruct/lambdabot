{-# LANGUAGE OverloadedStrings #-}

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
import qualified Data.Text as T

type Definition = (T.Text, T.Text)
type Vote = (T.Text, Int)

data GameState = MkGameState
  { server :: T.Text
  , channel :: T.Text
  , name :: T.Text
  , botName :: T.Text
  , story :: Definition
  , votes :: [Vote]
  }
  deriving (Show, Read)

data StoryState = MkStoryState
  { games :: [(T.Text, GameState)]
  , stories :: [Definition]
  , messageStoryAdded :: T.Text
  , messageStoryRemoved :: T.Text
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

newGameState :: T.Text -> Nick -> Nick -> Definition -> GameState
newGameState srvr nick botn chosenStory =
  MkGameState
    { server = srvr
    , channel = T.pack $ nName nick
    , name = T.pack $ nTag nick
    , botName = T.pack $ nName botn
    , story = chosenStory
    , votes = []
    }
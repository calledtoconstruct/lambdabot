module Lambdabot.Plugin.Term.Configuration (
  TermState (TermState),
  terms,
  lockedTerms,
  lastUnlockedTerm,
  messageTermAdded,
  messageTermRemoved,
  newTermState,
  Definition,
) where

import Lambdabot.Util (io)

import Control.Monad.Trans (MonadIO)
import Data.Time.Clock.System (SystemTime (systemSeconds), getSystemTime)
import GHC.Int (Int64)

type Definition = ([String], String)

data TermState = TermState
  { terms :: [Definition]
  , lockedTerms :: [String]
  , lastUnlockedTerm :: Int64
  , messageTermAdded :: String
  , messageTermRemoved :: String
  }
  deriving (Show, Read)

newTermState :: MonadIO m => [Definition] -> m TermState
newTermState allTheTerms = do
  time <- io getSystemTime
  pure
    TermState
      { terms = allTheTerms
      , lockedTerms = []
      , lastUnlockedTerm = systemSeconds time
      , messageTermAdded = "Term added"
      , messageTermRemoved = "Term removed"
      }

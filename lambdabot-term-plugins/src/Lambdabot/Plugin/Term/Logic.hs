module Lambdabot.Plugin.Term.Logic where

import Lambdabot.Config.Term (termFrequency)
import Lambdabot.Plugin (MonadConfig (getConfig), randomElem)
import Lambdabot.Plugin.Term.Configuration (
  Channel (..),
  ChannelName,
  TermDescription,
  TermName,
  TermState (..),
  addChannel,
 )

import Control.Monad.Trans (MonadIO)
import Data.Char (isAlpha, isNumber, isPunctuation)
import Data.List (partition)
import Data.Maybe (fromJust, isJust)
import Text.ParserCombinators.ReadP (ReadP, munch1, readP_to_S, sepBy)

data StateChangeResult = StateChangeResult
  { newState :: TermState
  , message :: String
  }

addTerm :: MonadIO m => TermState -> ChannelName -> TermName -> TermDescription -> m StateChangeResult
addTerm termState nameOfThisChannel term definition =
  let (channelTermState, otherChannels) = partition ((== nameOfThisChannel) . channelName) $ channels termState
   in if null channelTermState
        then do
          (updatedState, newChannel) <- addChannel termState nameOfThisChannel
          pure
            StateChangeResult
              { newState = updatedState
              , message = messageTermAdded newChannel
              }
        else
          let thisChannelTermState = head channelTermState
              existingTerms = filter (notElem term . fst) $ terms thisChannelTermState
           in pure
                StateChangeResult
                  { message = messageTermAdded thisChannelTermState
                  , newState =
                      termState
                        { channels =
                            thisChannelTermState
                              { terms = ([term], definition) : existingTerms
                              } :
                            otherChannels
                        }
                  }

removeTerm :: TermState -> ChannelName -> TermName -> StateChangeResult
removeTerm termState nameOfThisChannel term =
  let (channelTermState, otherChannels) = partition ((== nameOfThisChannel) . channelName) $ channels termState
   in if null channelTermState
        then
          StateChangeResult
            { newState = termState
            , message = "Remove Term :: Channel not found!"
            }
        else
          let thisChannel = head channelTermState
           in StateChangeResult
                { message = messageTermRemoved thisChannel
                , newState =
                    termState
                      { channels =
                          thisChannel
                            { terms = filter (notElem term . fst) $ terms thisChannel
                            } :
                          otherChannels
                      }
                }

data FindTermResult = FindTermResult
  { foundTerm :: TermName
  , foundDefinition :: TermDescription
  }

{- | Parse a sentence skipping spaces and punctuation.

 >>> fst . last $ readP_to_S sentenceParser "Thank You!"
 ["Thank","You"]
 >>> fst . last $ readP_to_S sentenceParser "We will go to Tess' new (fancy) A-Frame house."
 ["We","will","go","to","Tess'","new","fancy","A-Frame","house"]
-}
sentenceParser :: ReadP [String]
sentenceParser = token `sepBy` spacesAndPunctuation

{- | Parse spaces and punctuation

 >>> last $ readP_to_S spacesAndPunctuation " ?-!.()#%"
 (" ?-!.()#%","")
-}
spacesAndPunctuation :: ReadP String
spacesAndPunctuation = munch1 (\c -> c == ' ' || isPunctuation c)

token :: ReadP String
token = munch1 (\c -> isAlpha c || isNumber c || elem c "-\'")

findTerm :: MonadConfig m => MonadIO m => TermState -> ChannelName -> TermName -> m (Maybe FindTermResult)
findTerm termState nameOfThisChannel msg =
  let channelTermState = filter ((== nameOfThisChannel) . channelName) $ channels termState
   in if null channelTermState
        then pure Nothing
        else
          let thisChannel = head channelTermState
              recent = lockedTerms thisChannel
              spokenWords = fst . last $ readP_to_S sentenceParser msg
              mts = filter (`elem` spokenWords) $ filter (`notElem` recent) $ concatMap fst $ terms thisChannel
           in pure
                =<< ( \mt doIt ->
                        if doIt && isJust mt
                          then Just $ quoteTerm thisChannel $ fromJust mt
                          else Nothing
                    )
                <$> ( case length mts of
                        0 -> pure Nothing
                        1 -> pure $ Just $ head mts
                        _ -> Just <$> randomElem mts
                    )
                <*> shouldQuoteTerm

shouldQuoteTerm :: MonadIO m => MonadConfig m => m Bool
shouldQuoteTerm = do
  (frequency, outOf) <- getConfig termFrequency
  notTooChatty <- randomElem [1 .. (abs outOf)]
  pure $ notTooChatty <= frequency

quoteTerm :: Channel -> TermName -> FindTermResult
quoteTerm thisChannel mt =
  FindTermResult
    { foundTerm = mt
    , foundDefinition = snd $ head $ filter (elem mt . fst) $ terms thisChannel
    }

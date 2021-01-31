module Lambdabot.Plugin.Term.Logic where

import Lambdabot.Config.Term (termFrequency)
import Lambdabot.Plugin (MonadConfig (getConfig))
import Lambdabot.Plugin.Term.Configuration (
  TermState (
    lockedTerms,
    messageTermAdded,
    messageTermRemoved,
    terms
  ),
 )
import Lambdabot.Util (io, random)

import Control.Monad.Trans (MonadIO)
import Data.Maybe (fromJust, isJust)
import Data.Char (isAlpha, isNumber, isPunctuation)
import Text.ParserCombinators.ReadP (readP_to_S, sepBy, munch1, ReadP)

data StateChangeResult = StateChangeResult
  { newState :: TermState
  , message :: String
  }

addTerm :: TermState -> String -> String -> StateChangeResult
addTerm termState term definition =
  let existingTerms = filter (notElem term . fst) $ terms termState
   in StateChangeResult
        { message = messageTermAdded termState
        , newState =
            termState
              { terms = ([term], definition) : existingTerms
              }
        }

removeTerm :: TermState -> String -> StateChangeResult
removeTerm termState term =
  StateChangeResult
    { message = messageTermRemoved termState
    , newState =
        termState
          { terms = filter (notElem term . fst) $ terms termState
          }
    }

data FindTermResult = FindTermResult
  { foundTerm :: String
  , foundDefinition :: String
  }

-- | Parse a sentence skipping spaces and punctuation.
--
-- >>> fst . last $ readP_to_S sentenceParser "Thank You!"
-- ["Thank","You"]
-- >>> fst . last $ readP_to_S sentenceParser "We will go to Tess' new (fancy) A-Frame house."
-- ["We","will","go","to","Tess'","new","fancy","A-Frame","house"]
--
sentenceParser :: ReadP [String]
sentenceParser = token `sepBy` spacesAndPunctuation

-- | Parse spaces and punctuation
--
-- >>> last $ readP_to_S spacesAndPunctuation " ?-!.()#%"
-- (" ?-!.()#%","")
--
spacesAndPunctuation :: ReadP String
spacesAndPunctuation = munch1 (\c -> c == ' ' || isPunctuation c)

token :: ReadP String
token = munch1 (\c-> isAlpha c || isNumber c || elem c "-\'")

findTerm :: MonadConfig m => MonadIO m => TermState -> String -> m (Maybe FindTermResult)
findTerm termState msg = do
  let recent = lockedTerms termState
  let spokenWords = fst . last $ readP_to_S sentenceParser msg
  let mts = filter (`elem` spokenWords) $ filter (`notElem` recent) $ concatMap fst $ terms termState
  x <- io $ random mts
  doIt <- shouldQuoteTerm
  let mt = case length mts of
        0 -> Nothing
        1 -> Just $ head mts
        _ -> Just x
  if doIt && isJust mt
    then pure $ Just $ quoteTerm termState $ fromJust mt
    else pure Nothing

shouldQuoteTerm :: MonadIO m => MonadConfig m => m Bool
shouldQuoteTerm = do
  (frequency, outOf) <- getConfig termFrequency
  notTooChatty <- random [1 .. (abs outOf)]
  pure $ notTooChatty <= frequency

quoteTerm :: TermState -> String -> FindTermResult
quoteTerm termState mt =
  FindTermResult
    { foundTerm = mt
    , foundDefinition = snd $ head $ filter (elem mt . fst) $ terms termState
    }

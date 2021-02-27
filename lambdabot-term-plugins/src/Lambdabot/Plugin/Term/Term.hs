module Lambdabot.Plugin.Term.Term (termPlugin) where

import Lambdabot.Config.Term (defaultTerms, secondsToWaitBeforeUnlockingTerm)
import Lambdabot.Plugin (
  Cmd,
  LB,
  Module,
  ModuleT,
  MonadConfig (getConfig),
  aliases,
  command,
  contextual,
  getTarget,
  help,
  io,
  moduleCmds,
  moduleDefState,
  moduleInit,
  moduleSerialize,
  nName,
  newModule,
  privileged,
  process,
  say,
  stdSerial,
  withMS,
 )
import Lambdabot.Plugin.Term.Configuration (Channel (lastUnlockedTerm, lockedTerms), TermState (channels), channelName, newTermState, terms)
import Lambdabot.Plugin.Term.Logic (FindTermResult (foundDefinition, foundTerm), addTerm, findTerm, message, newState, removeTerm)

import Control.Monad (unless, when)
import Data.List (partition)
import Data.Time.Clock.System (SystemTime (systemSeconds), getSystemTime)
import Lambdabot.Logging (debugM)

type Term = ModuleT TermState LB

termPlugin :: Module TermState
termPlugin =
  newModule
    { moduleSerialize = Just stdSerial
    , moduleDefState = newTermState <$> getConfig defaultTerms
    , moduleInit = return ()
    , moduleCmds =
        return
          [ (command "term")
              { help = say "term <term> - Show the specified term."
              , process = commandShowTerm
              }
          , (command "add-term")
              { help = say "add-term <term> <definition> - Add a new term to the database."
              , process = commandAddTerm
              , privileged = True
              }
          , (command "remove-term")
              { aliases = ["rm-term"]
              , help = say "remove-term <term> - Remove a term from the database."
              , process = commandRemoveTerm
              , privileged = True
              }
          ]
    , contextual = commandContextual
    }

commandContextual :: String -> Cmd Term ()
commandContextual msg = withMS $ \termState writer -> do
  thisChannelName <- nName <$> getTarget
  debugM $ "Contextual for channel name :: " ++ thisChannelName
  let (thisChannelTermState, otherChannels) = partition ((== thisChannelName) . channelName) $ channels termState
  unless (null thisChannelTermState) $ do
    found <- findTerm termState thisChannelName msg
    currentTime <- io getSystemTime
    secondsToWait <- getConfig secondsToWaitBeforeUnlockingTerm
    let thisChannel = head thisChannelTermState
    let currentTimeInSeconds = systemSeconds currentTime
    let seconds = currentTimeInSeconds - lastUnlockedTerm thisChannel
    let itHasBeenLongEnough = seconds > secondsToWait
    let termCanBeUnlocked = not $ null $ lockedTerms thisChannel
    case found of
      Just result -> do
        let term = foundTerm result
        let updatedChannel = thisChannel{lockedTerms = term : lockedTerms thisChannel}
        writer
          termState
            { channels = updatedChannel : otherChannels
            }
        say $ term ++ " :: " ++ foundDefinition result
      Nothing ->
        when (itHasBeenLongEnough && termCanBeUnlocked) $
          let updatedChannel =
                thisChannel
                  { lockedTerms = init $ lockedTerms thisChannel
                  , lastUnlockedTerm = currentTimeInSeconds
                  }
           in writer
                termState
                  { channels = updatedChannel : otherChannels
                  }

commandShowTerm :: String -> Cmd Term ()
commandShowTerm term = withMS $ \termState _ -> do
  thisChannelName <- nName <$> getTarget
  let thisChannelTermState = filter ((== thisChannelName) . channelName) $ channels termState
  unless (null thisChannelTermState) $
    let foundItems = filter (elem term . fst) $ terms $ head thisChannelTermState
     in case foundItems of
          [found] -> say $ term ++ " :: " ++ snd found
          _ -> say $ "I don't know about " ++ term

commandAddTerm :: String -> Cmd Term ()
commandAddTerm txt' = do
  x  <- maybeGetChannelNameFromText (words txt') . nName <$> getTarget
  case x of
    Just ~(thisChannelName, term : definition) -> withMS $ \termState writer -> do
      result <- addTerm termState thisChannelName term $ unwords definition
      writer $ newState result
      sayMessage $ message result
    Nothing -> say "Not enough parameters.  add-term help for more information."

maybeGetChannelNameFromText :: [String] -> String -> Maybe (String, [String])
maybeGetChannelNameFromText wrds thisChannelName'
  | length wrds > 2 && "offlinerc" == thisChannelName' = Just (head wrds, tail wrds)
  | length wrds > 1 = Just (thisChannelName', wrds)
  | otherwise = Nothing

commandRemoveTerm :: String -> Cmd Term ()
commandRemoveTerm [] = say "Not enough parameters.  remove-term <term>"
commandRemoveTerm term = withMS $ \termState writer -> do
  thisChannelName <- nName <$> getTarget
  let result = removeTerm termState thisChannelName term
  writer $ newState result
  sayMessage $ message result

sayMessage :: String -> Cmd Term ()
sayMessage [] = return ()
sayMessage output = say output

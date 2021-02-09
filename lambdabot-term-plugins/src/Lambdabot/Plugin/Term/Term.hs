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
  help,
  io,
  moduleCmds,
  moduleDefState,
  moduleInit,
  moduleSerialize,
  newModule,
  privileged,
  process,
  say,
  stdSerial,
  withMS,
 )
import Lambdabot.Plugin.Term.Configuration (TermState (lastUnlockedTerm, lockedTerms), newTermState, terms)
import Lambdabot.Plugin.Term.Logic (FindTermResult (foundDefinition, foundTerm), addTerm, findTerm, message, newState, removeTerm)

import Control.Monad (when)
import Data.Time.Clock.System (SystemTime (systemSeconds), getSystemTime)

type Term = ModuleT TermState LB

termPlugin :: Module TermState
termPlugin =
  newModule
    { moduleSerialize = Just stdSerial
    , moduleDefState = do
        initialTerms <- getConfig defaultTerms
        newTermState initialTerms
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
commandContextual msg = do
  withMS $ \termState writer -> do
    found <- findTerm termState msg
    currentTime <- io getSystemTime
    secondsToWait <- getConfig secondsToWaitBeforeUnlockingTerm
    let currentTimeInSeconds = systemSeconds currentTime
    let seconds = currentTimeInSeconds - lastUnlockedTerm termState
    let itHasBeenLongEnough = seconds > secondsToWait
    let termCanBeUnlocked = not $ null $ lockedTerms termState
    case found of
      Just result -> do
        let term = foundTerm result
        writer
          termState
            { lockedTerms = term : lockedTerms termState
            }
        say $ term ++ " :: " ++ foundDefinition result
      Nothing -> when (itHasBeenLongEnough && termCanBeUnlocked) $ do
        writer
          termState
            { lockedTerms = init $ lockedTerms termState
            , lastUnlockedTerm = currentTimeInSeconds
            }

commandShowTerm :: String -> Cmd Term ()
commandShowTerm term = do
  withMS $ \termState _ ->
    let foundItems = filter (elem term . fst) $ terms termState
     in case foundItems of
          [found] -> say $ term ++ " :: " ++ snd found
          _ -> say $ "I don't know about " ++ term

commandAddTerm :: String -> Cmd Term ()
commandAddTerm [] = say "Not enough parameters.  add-term help for more information."
commandAddTerm txt = do
  let term : definition = words txt
  if null definition
    then say $ "Not enough parameters.  Please provide a definition for " ++ term
    else withMS $ \termState writer -> do
      let result = addTerm termState term $ unwords definition
      writer $ newState result
      sayMessage $ message result

commandRemoveTerm :: String -> Cmd Term ()
commandRemoveTerm [] = say "Not enough parameters.  remove-term <term>"
commandRemoveTerm term =
  withMS $ \termState writer -> do
    let result = removeTerm termState term
    writer $ newState result
    sayMessage $ message result

sayMessage :: String -> Cmd Term ()
sayMessage [] = return ()
sayMessage output = say output

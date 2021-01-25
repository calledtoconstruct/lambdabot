{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

{- | Module: Poll
 | Support for voting / polling
 |
 | License: lGPL
 |
 | added by Kenneth Hoste (boegel), 22/11/2005
 |  inspiration: Where plugin (thanks shapr,dons)
 | modified by Joseph Woolley, 2021-01-25
-}
module Lambdabot.Plugin.Social.Poll (pollPlugin) where

import Lambdabot.Plugin (
  Cmd,
  Command (aliases, help, privileged, process),
  LB,
  Module (moduleCmds, moduleDefState, moduleSerialize),
  ModuleT,
  MonadLBState (withMS),
  Packable (..),
  Serial (Serial),
  command,
  newModule,
  readPackedEntry,
  say,
 )

import Data.Bifunctor (Bifunctor (first))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as P
import Data.List (intercalate)
import qualified Data.Map as M

newPoll :: Poll
newPoll = ((Created, FixedChoices), [])

appendPoll :: String -> Poll -> Maybe Poll
appendPoll choice (o, ls) = Just (o, (choice, 0) : ls)

voteOnPoll :: ChoiceState -> Poll -> String -> (Poll, String)
voteOnPoll cs (o, poll) choice =
  if matched || cs == AddYourOwn
    then
      (
        ( o
        , map
            ( \(c, n) ->
                if c == choice
                  then (c, n + 1)
                  else (c, n)
            )
            poll
        )
      , "voted on " ++ show choice
      )
    else ((o, poll), show choice ++ " is not an option for this poll.  Here are the options: " ++ available)
 where
  matched = any (\(x, _) -> x == choice) poll
  available = show $ map fst poll

------------------------------------------------------------------------

data PollState = Created | Active | Closed deriving (Eq, Show, Read)
data ChoiceState = AddYourOwn | FixedChoices deriving (Eq, Show, Read)
type Count = Int
type Candidate = String
type PollName = P.ByteString
type Poll = ((PollState, ChoiceState), [(Candidate, Count)])
type VoteState = M.Map PollName Poll
type VoteWriter = VoteState -> Cmd Vote ()
type Vote = ModuleT VoteState LB

------------------------------------------------------------------------
-- Define a serialiser

voteSerial :: Serial VoteState
voteSerial = Serial (Just . showPacked) (Just . readPacked)

instance Packable (M.Map ByteString ((PollState, ChoiceState), [(String, Int)])) where
  readPacked = M.fromList . readPackedEntry (splitAt 2) (\(k : v : _) -> (k, read . P.unpack $ v)) . P.lines
  showPacked m = P.unlines . concatMap (\(k, v) -> [k, P.pack . show $ v]) $ M.toList m

------------------------------------------------------------------------

pollPlugin :: Module (M.Map PollName Poll)
pollPlugin =
  newModule
    { moduleCmds =
        return
          [ (command "poll-list")
              { help = say "poll-list                   Shows all current polls"
              , aliases = ["polls"]
              , process = processZeroOrMoreArgs "poll-list"
              }
          , (command "poll-show")
              { help = say "poll-show <poll>            Shows all choices for some poll"
              , aliases = ["poll"]
              , process = processZeroOrMoreArgs "poll-show"
              }
          , (command "poll-add")
              { help = say "poll-add <name>             Adds a new poll, with no choices"
              , process = processOneOrMoreArgs "poll-add"
              , privileged = True
              }
          , (command "choice-add")
              { help = say "choice-add <poll> <choice>  Adds a new choice to the given poll"
              , process = processOneOrMoreArgs "choice-add"
              , privileged = True
              }
          , (command "vote")
              { help = say "vote <poll> <choice>        Vote for <choice> in <poll>"
              , process = processOneOrMoreArgs "vote"
              }
          , (command "poll-result")
              { help = say "poll-result <poll>          Show result for given poll"
              , process = processOneOrMoreArgs "poll-result"
              }
          , (command "poll-choices")
              { help = say "poll-choices <poll> (any | fixed)      Allows or prevents the adding of choices during voting"
              , process = processOneOrMoreArgs "poll-choices"
              , privileged = True
              }
          , (command "poll-open")
              { help = say "poll-open <poll>            Opens a poll"
              , process = processOneOrMoreArgs "poll-open"
              , privileged = True
              }
          , (command "poll-close")
              { help = say "poll-close <poll>           Closes a poll"
              , process = processOneOrMoreArgs "poll-close"
              , privileged = True
              }
          , (command "poll-remove")
              { help = say "poll-remove <poll>          Removes a poll"
              , process = processOneOrMoreArgs "poll-remove"
              , privileged = True
              }
          , (command "poll-reset")
              { help = say "poll-reset <poll>           Resets poll and vote counts"
              , process = processOneOrMoreArgs "poll-reset"
              , privileged = True
              }
          ]
    , moduleDefState = return M.empty
    , moduleSerialize = Just voteSerial
    }

processOneOrMoreArgs :: [Char] -> [Char] -> Cmd Vote ()
processOneOrMoreArgs cmd [] = say ("Missing argument. Check ?help " ++ cmd ++ " for info.")
processOneOrMoreArgs cmd dat = processZeroOrMoreArgs cmd dat

processZeroOrMoreArgs :: [Char] -> [Char] -> Cmd Vote ()
processZeroOrMoreArgs cmd dat = do
  result <- withMS $ \voteState voteWriter -> processCommand voteState voteWriter cmd (words dat)
  say result

------------------------------------------------------------------------

processCommand :: VoteState -> VoteWriter -> String -> [String] -> Cmd Vote String
processCommand voteState voteWriter cmd dat = case cmd of
  -- show all current polls
  "poll-list" -> return $ listPolls voteState
  -- show candidates
  "poll-show" -> return $ case length dat of
    0 -> showActivePolls voteState []
    1 -> showActivePolls voteState $ head dat
    _ -> "usage: ?poll-show <poll>"
  -- declare a new poll
  "poll-add" -> case length dat of
    1 -> addPoll voteState voteWriter pollName
    _ -> return "usage: ?poll-add <poll>   with \"ThisTopic\" style names"
  "choice-add" -> case length dat of
    2 -> addChoice voteState voteWriter pollName $ last dat
    _ -> return "usage: ?choice-add <poll> <choice>"
  "vote" -> case length dat of
    2 -> vote voteState voteWriter pollName $ last dat
    _ -> return "usage: ?vote <poll> <choice>"
  "poll-result" -> return $ case length dat of
    1 -> showResult voteState pollName
    _ -> "usage: ?poll-result <poll>"
  "poll-choices" -> case length dat of
    1 -> choices voteState voteWriter pollName []
    2 -> choices voteState voteWriter pollName $ last dat
    _ -> return "usage: ?poll-choices <poll> (any | fixed)"
  "poll-open" -> case length dat of
    1 -> openPoll voteState voteWriter pollName
    _ -> return "usage: ?poll-open <poll>"
  "poll-close" -> case length dat of
    1 -> closePoll voteState voteWriter pollName
    _ -> return "usage: ?poll-close <poll>"
  "poll-remove" -> case length dat of
    1 -> removePoll voteState voteWriter pollName
    _ -> return "usage: ?poll-remove <poll>"
  "poll-reset" -> case length dat of
    1 -> resetPoll voteState voteWriter pollName
    _ -> return "usage: @poll-reset <poll>"
  _ -> return "Unknown command."
 where
  pollName = P.pack $ head dat

------------------------------------------------------------------------

listPolls :: VoteState -> String
listPolls voteState = show $ map fst $ M.toList voteState

showPoll :: (String, Poll) -> String
showPoll (pn, poll) = case poll of
  ((Created, _), p) -> "Poll " ++ pn ++ " is not yet ready.  Votes will be accepted once the poll is opened.  Current choices include: " ++ intercalate ", " (map ppr p)
  ((Active, _), p) -> "Poll " ++ pn ++ " is active.  Cast your vote now!  Currently, the votes are: " ++ intercalate ", " (map ppr p)
  ((Closed, _), p) -> "Poll " ++ pn ++ " is now closed.  The results are: " ++ intercalate ", " (map ppr p)
 where
  ppr (x, y) = x ++ "=" ++ show y

showActivePolls :: VoteState -> String -> String
showActivePolls voteState []
  | null activePolls = "There are no active polls at the moment.  Consider suggesting one!"
  | otherwise = concatMap (showPoll . first P.unpack) activePolls
 where
  activeOnly = (==) Active . fst . fst . snd
  activePolls = filter activeOnly $ M.toList voteState
showActivePolls voteState poll = case M.lookup (P.pack poll) voteState of
  Nothing -> "No such poll: " ++ show poll ++ " Use ?poll-list to see the available polls."
  Just p -> showPoll (poll, p)

addPoll :: VoteState -> VoteWriter -> ByteString -> Cmd Vote String
addPoll voteState voteWriter poll = case M.lookup poll voteState of
  Nothing -> do
    voteWriter $ M.insert poll newPoll voteState
    return $ "Added new poll: " ++ show poll
  Just _ -> return $ "Poll " ++ show poll ++ " already exists, choose another name for your poll"

addChoice :: VoteState -> VoteWriter -> ByteString -> String -> Cmd Vote String
addChoice voteState voteWriter poll choice = case M.lookup poll voteState of
  Nothing -> return $ "No such poll: " ++ show poll
  Just _ -> do
    voteWriter $ M.update (appendPoll choice) poll voteState
    return $ "New candidate " ++ show choice ++ ", added to poll " ++ show poll ++ "."

vote :: VoteState -> VoteWriter -> ByteString -> String -> Cmd Vote String
vote voteState voteWriter poll choice = case M.lookup poll voteState of
  Nothing -> return $ "No such poll: " ++ show poll
  Just ((Created, _), _) -> return $ "The " ++ show poll ++ " poll is not yet active, sorry !"
  Just ((Closed, _), _) -> return $ "The " ++ show poll ++ " poll is closed, sorry !"
  Just p@((Active, cs), _) -> do
    let (np, msg) = voteOnPoll cs p choice
    voteWriter $ M.update (const (Just np)) poll voteState
    return msg

showResult :: VoteState -> ByteString -> String
showResult voteState pollName = case M.lookup pollName voteState of
  Nothing -> "No such poll: " ++ show pollName
  Just poll -> showPoll (P.unpack pollName, poll)

removePoll :: VoteState -> VoteWriter -> ByteString -> Cmd Vote String
removePoll voteState voteWriter poll = case M.lookup poll voteState of
  Just ((Active, _), _) -> return "Poll should be closed before you remove it."
  Just ((Closed, _), _) -> removeIt
  Just ((Created, _), _) -> removeIt
  Nothing -> return $ "No such poll: " ++ show poll
 where
  removeIt :: Cmd Vote String
  removeIt = do
    voteWriter $ M.delete poll voteState
    return $ "poll " ++ show poll ++ " removed."

choices :: VoteState -> VoteWriter -> ByteString -> String -> Cmd Vote String
choices voteState voteWriter poll value = case M.lookup poll voteState of
  Nothing -> return $ "No such poll: " ++ show poll
  Just ((ps, cs), p) -> do
    voteWriter $ M.update (const (Just ((ps, ncs), p))) poll voteState
    return $ "Poll " ++ show poll ++ " is now set to " ++ choiceState
   where
    ncs
      | null value && cs == AddYourOwn = FixedChoices
      | null value && cs == FixedChoices = AddYourOwn
      | value == "any" = AddYourOwn
      | value == "fixed" = FixedChoices
      | otherwise = FixedChoices
    choiceState
      | ncs == AddYourOwn = "allow any choice."
      | otherwise = "allow only choices on the list."

openPoll :: VoteState -> VoteWriter -> ByteString -> Cmd Vote String
openPoll voteState voteWriter poll = case M.lookup poll voteState of
  Nothing -> return $ "No such poll: " ++ show poll
  Just ((_, cs), p) -> do
    voteWriter $ M.update (const (Just ((Active, cs), p))) poll voteState
    return $ "Poll " ++ show poll ++ " is now open.  Use ?vote <poll> <choice> to cast your vote!"

closePoll :: VoteState -> VoteWriter -> ByteString -> Cmd Vote String
closePoll voteState voteWriter poll = case M.lookup poll voteState of
  Nothing -> return $ "No such poll: " ++ show poll
  Just ((_, cs), p) -> do
    voteWriter $ M.update (const (Just ((Closed, cs), p))) poll voteState
    return $ "Poll " ++ show poll ++ " closed."

resetPoll :: VoteState -> VoteWriter -> ByteString -> Cmd Vote String
resetPoll fm writer poll = case M.lookup poll fm of
  Just ((_, cs), p) -> do
    let resetChoices = map (\(c, _) -> (c, 0)) p
    let updatedPoll = const (Just ((Created, cs), resetChoices))
    writer $ M.update updatedPoll poll fm
    return $ "Poll " ++ show poll ++ " reset."
  Nothing -> return $ "No such poll: " ++ show poll

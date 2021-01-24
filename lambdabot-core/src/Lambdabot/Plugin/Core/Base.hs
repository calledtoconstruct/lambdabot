{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternGuards #-}

module Lambdabot.Plugin.Core.Base (basePlugin) where

import Lambdabot.Bot (checkIgnore, checkPrivs, ircPrivmsg)
import Lambdabot.Command (lineify, Command (privileged), execCmd, runCommand)
import Lambdabot.Config.Core (commandPrefixes, disabledCommands, editDistanceLimit)
import Lambdabot.IRC (IrcMessage (ircMsgCommand, ircMsgParams, ircMsgPrefix, ircMsgServer), getTopic, joinChannel, timeReply)
import Lambdabot.Logging (debugM, noticeM)
import Lambdabot.Message (Message (lambdabotName, nick, server))
import Lambdabot.Module (IRCRWState (ircChannels, ircCommands, ircPersists), LB, Module (contextual, moduleDefState, moduleInit), ModuleInfo (moduleName, theModule), ModuleT, newModule)
import Lambdabot.Monad (MonadLB (lb), registerCallback, registerOutputFilter, send, withAllModules, withCommand)
import Lambdabot.Nick (Nick (Nick, nTag), fmtNick, parseNick)
import Lambdabot.Plugin (GlobalPrivate, MonadConfig (..), getCN, mkCN, mkGlobalPrivate, withPS)
import Lambdabot.Util (arePrefixesOf, dropFromEnd, expandTab, showClean, splitFirstWord)

import Control.Exception.Lifted as E (SomeException (SomeException), catch)
import Control.Monad.Reader (asks)
import Control.Monad.State (MonadTrans (lift), forM_, gets, modify, when)
import Data.Char (isSpace)
import Data.List (isPrefixOf, nub)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Text.EditDistance (defaultEditCosts, levenshteinDistance)
import Text.Regex.TDFA (MatchResult (mrAfter), Regex, RegexContext (matchM), RegexMaker (makeRegex))

type BaseState = GlobalPrivate () ()

type Base = ModuleT BaseState LB

basePlugin :: Module (GlobalPrivate () ())
basePlugin =
  newModule
    { moduleDefState = return $ mkGlobalPrivate 20 ()
    , moduleInit = do
        registerOutputFilter cleanOutput
        registerOutputFilter $ \_ value -> do
          reply <- lineify value
          pure $ lines reply
        registerOutputFilter cleanOutput

        registerCallback "PING" handlePingMessage
        registerCallback "NOTICE" handleNoticeMessage
        registerCallback "PART" handlePartMessage
        registerCallback "KICK" handleKickMessage
        registerCallback "JOIN" handleJoinMessage
        registerCallback "NICK" handleNickMessage
        registerCallback "MODE" handleModeMessage
        registerCallback "TOPIC" handleTopicMessage
        registerCallback "QUIT" handleQuitMessage
        registerCallback "PRIVMSG" doPRIVMSG
        registerCallback "001" handleReplyWelcomeMessage

        -- registerCallback "002"     doRPL_YOURHOST
        -- registerCallback "003"     doRPL_CREATED
        -- registerCallback "004"     doRPL_MYINFO

        registerCallback "005" handleReplyBounceMessage

        -- registerCallback "250"     doRPL_STATSCONN
        -- registerCallback "251"     doRPL_LUSERCLIENT
        -- registerCallback "252"     doRPL_LUSEROP
        -- registerCallback "253"     doRPL_LUSERUNKNOWN
        -- registerCallback "254"     doRPL_LUSERCHANNELS
        -- registerCallback "255"     doRPL_LUSERME
        -- registerCallback "265"     doRPL_LOCALUSERS
        -- registerCallback "266"     doRPL_GLOBALUSERS

        registerCallback "332" handleReplyTopicMessage

        -- registerCallback "353"     doRPL_NAMRELY
        -- registerCallback "366"     doRPL_ENDOFNAMES
        -- registerCallback "372"     doRPL_MOTD
        -- registerCallback "375"     doRPL_MOTDSTART
        -- registerCallback "376"     doRPL_ENDOFMOTD
    }

handleIgnoreMessage :: IrcMessage -> Base ()
handleIgnoreMessage = debugM . show

handlePingMessage :: IrcMessage -> Base ()
handlePingMessage = noticeM . showPingMsg
 where
  showPingMsg msg = "PING! <" ++ ircMsgServer msg ++ (':' : ircMsgPrefix msg) ++ "> [" ++ ircMsgCommand msg ++ "] " ++ show (ircMsgParams msg)

-- If this is a "TIME" then we need to pass it over to the localtime plugin
-- otherwise, dump it to stdout
handleNoticeMessage :: IrcMessage -> Base ()
handleNoticeMessage msg
  | isCTCPTimeReply = doPRIVMSG (timeReply msg)
  -- TODO: need to say which module to run the privmsg in
  | otherwise = noticeM (show body)
 where
  body = ircMsgParams msg
  isCTCPTimeReply = ":\SOHTIME" `isPrefixOf` last body

-- the empty topic causes problems
-- TODO: find out what they are and fix them properly
handleJoinMessage :: IrcMessage -> Base ()
handleJoinMessage msg
  | lambdabotName msg /= nick msg = handleIgnoreMessage msg
  | otherwise = do
    let msgArg = concat (take 1 (ircMsgParams msg))
    let chan = case dropWhile (/= ':') msgArg of
          [] -> msgArg
          aloc -> aloc
    let loc = Nick (server msg) (dropWhile (== ':') chan)
    lb . modify $ \s ->
      s
        { ircChannels = M.insert (mkCN loc) "[currently unknown]" (ircChannels s)
        }
    lb . send $ getTopic loc -- initialize topic

handlePartMessage :: IrcMessage -> Base ()
handlePartMessage msg = when (lambdabotName msg == nick msg) $ do
  let body = ircMsgParams msg
  let loc = Nick (server msg) (head body)
  lb . modify $ \s ->
    s
      { ircChannels = M.delete (mkCN loc) (ircChannels s)
      }

handleKickMessage :: IrcMessage -> Base ()
handleKickMessage msg = do
  let body = ircMsgParams msg
  let loc = Nick (server msg) (head body)
  let who = Nick (server msg) (body !! 1)
  when (lambdabotName msg == who) $ do
    noticeM $ fmtNick "" (nick msg) ++ " KICK " ++ fmtNick (server msg) loc ++ " " ++ show (drop 2 body)
    lift $
      modify $ \s ->
        s
          { ircChannels = M.delete (mkCN loc) (ircChannels s)
          }

handleNickMessage :: IrcMessage -> Base ()
handleNickMessage = handleIgnoreMessage

handleModeMessage :: IrcMessage -> Base ()
handleModeMessage = handleIgnoreMessage

handleTopicMessage :: IrcMessage -> Base ()
handleTopicMessage msg = lb . modify $ \s ->
  s
    { ircChannels = M.insert (mkCN loc) (tail $ head $ tail $ ircMsgParams msg) (ircChannels s)
    }
 where
  loc = Nick (server msg) (head (ircMsgParams msg))

handleReplyWelcomeMessage :: IrcMessage -> Base ()
handleReplyWelcomeMessage msg = lb $ do
  modify $ \state' -> let persists = if M.findWithDefault True (server msg) (ircPersists state') then ircPersists state' else M.delete (server msg) $ ircPersists state' in state'{ircPersists = persists}
  chans <- gets ircChannels
  forM_ (M.keys chans) $ \chan -> do
    let cn = getCN chan
    when (nTag cn == server msg) $ do
      modify $ \state' ->
        state'
          { ircChannels = M.delete chan $ ircChannels state'
          }
    lb $ send $ joinChannel cn

handleQuitMessage :: IrcMessage -> Base ()
handleQuitMessage = handleIgnoreMessage

handleReplyBounceMessage :: IrcMessage -> Base ()
handleReplyBounceMessage _msg = debugM "BOUNCE!"

handleReplyTopicMessage :: IrcMessage -> Base ()
handleReplyTopicMessage msg = do
  let body = ircMsgParams msg
  let loc = Nick (server msg) (body !! 1)
  lb . modify $ \s ->
    s
      { ircChannels = M.insert (mkCN loc) (tail $ last body) (ircChannels s)
      }

doPRIVMSG :: IrcMessage -> Base ()
doPRIVMSG msg = do
  ignored <- lift $ checkIgnore msg
  commands <- getConfig commandPrefixes
  if ignored then handleIgnoreMessage msg else mapM_ (doPRIVMSG' commands (lambdabotName msg) msg) targets
 where
  alltargets = head (ircMsgParams msg)
  targets = map (parseNick (ircMsgServer msg)) $ splitOn "," alltargets

--

-- | What does the bot respond to?
doPRIVMSG' :: [String] -> Nick -> IrcMessage -> Nick -> Base ()
doPRIVMSG' commands myname msg target
  | myname == target =
    let (cmd, params) = splitFirstWord text
     in doPersonalMsg commands msg target text cmd params
  | flip any ":," $ \c -> (fmtNick (ircMsgServer msg) myname ++ [c]) `isPrefixOf` text =
    let Just wholeCmd = maybeCommand (fmtNick (ircMsgServer msg) myname) text
        (cmd, params) = splitFirstWord wholeCmd
     in doPublicMsg commands msg target cmd params
  | (commands `arePrefixesOf` text) && length text > 1 && (text !! 1 /= ' ') && (not (commands `arePrefixesOf` [text !! 1]) || (length text > 2 && text !! 2 == ' ')) = let (cmd, params) = splitFirstWord (dropWhile (== ' ') text) in doPublicMsg commands msg target cmd params
  | otherwise = doContextualMsg msg target target text
 where
  text = tail (head (tail (ircMsgParams msg)))

doPersonalMsg :: [String] -> IrcMessage -> Nick -> String -> String -> String -> Base ()
doPersonalMsg commands msg target text s r
  | commands `arePrefixesOf` s = doMsg msg (tail s) r who
  | otherwise = doContextualMsg msg target who text
 where
  who = nick msg

doPublicMsg :: [String] -> IrcMessage -> Nick -> String -> String -> Base ()
doPublicMsg commands msg target s r
  | commands `arePrefixesOf` s = doMsg msg (tail s) r target
  | otherwise = handleIgnoreMessage msg

--
-- normal commands.
--
-- check privledges, do any spell correction, dispatch, handling
-- possible timeouts.
--
-- todo, refactor
--
doMsg :: IrcMessage -> String -> String -> Nick -> Base ()
doMsg msg cmd rest towhere = do
  let ircmsg = ircPrivmsg towhere
  allcmds <- lift (gets (M.keys . ircCommands))
  let ms = filter (isPrefixOf cmd) allcmds
  e <- getConfig editDistanceLimit
  case ms of
    [s] -> docmd msg towhere rest s -- a unique prefix
    _ | cmd `elem` ms -> docmd msg towhere rest cmd -- correct command (usual case)
    _ | otherwise -> case closests cmd allcmds of
      (n, [s]) | n < e, null ms -> docmd msg towhere rest s -- unique edit match
      (n, ss) | n < e || ms /= [] -> lift . ircmsg $ "Maybe you meant: " ++ showClean (nub (ms ++ ss))
      _ -> docmd msg towhere rest cmd -- no prefix, edit distance too far

docmd :: IrcMessage -> Nick -> [Char] -> String -> Base ()
docmd msg towhere rest cmd' = withPS towhere $ \_ _ -> do
  withCommand
    cmd'
    (ircPrivmsg towhere "Unknown command, try ?list")
    ( \theCmd -> do
        name' <- asks moduleName
        hasPrivs <- lb $ checkPrivs msg

        -- TODO: handle disabled commands earlier
        -- users should probably see no difference between a
        -- command that is disabled and one that doesn't exist.
        disabled <- elem cmd' <$> getConfig disabledCommands
        let ok = not disabled && (not (privileged theCmd) || hasPrivs)
        response <-
          if not ok
            then return ["Not enough privileges"]
            else withTryCatch name' $ runCommand theCmd msg towhere cmd' rest

        -- send off our response strings
        -- TODO: expandTab here should probably be an OutputFilter
        lift $ mapM_ (ircPrivmsg towhere . expandTab 8) response
    )
 where
  withTryCatch name' x = x `E.catch` \exc@SomeException{} -> return ["Plugin `" ++ name' ++ "' failed with: " ++ show exc]

--
-- contextual messages are all input that isn't an explicit command.
-- they're passed to all modules (todo, sounds inefficient) for
-- scanning, and any that implement 'contextual' will reply.
--
-- we try to run the contextual functions from all modules, on every
-- non-command. better hope this is efficient.
--
-- Note how we catch any plugin errors here, rather than letting
-- them bubble back up to the mainloop
--
doContextualMsg :: IrcMessage -> Nick -> Nick -> [Char] -> Base ()
doContextualMsg msg target towhere r = lb $ withAllModules $ withHandler invokeContextual
 where
  withHandler x = E.catch x $ \e@SomeException{} -> do
    mName <- asks moduleName
    debugM ("Module " ++ show mName ++ " failed in contextual handler: " ++ show e)
  invokeContextual = do
    m <- asks theModule
    reply <- execCmd (contextual m r) msg target "contextual"
    lb $ mapM_ (ircPrivmsg towhere) reply

------------------------------------------------------------------------

closests :: String -> [String] -> (Int, [String])
closests pat ss = M.findMin m
 where
  m = M.fromListWith (++) ls
  ls = [(levenshteinDistance defaultEditCosts pat s, [s]) | s <- ss]

maybeCommand :: String -> String -> Maybe String
maybeCommand nm text = mrAfter <$> matchM re text
 where
  re :: Regex
  re = makeRegex (nm ++ "[.:,]*[[:space:]]*")

--
-- And stuff we don't care about
--

{-
doRPL_YOURHOST :: IrcMessage -> LB ()
doRPL_YOURHOST _msg = return ()

doRPL_CREATED :: IrcMessage -> LB ()
doRPL_CREATED _msg = return ()

doRPL_MYINFO :: IrcMessage -> LB ()
doRPL_MYINFO _msg = return ()

doRPL_STATSCONN :: IrcMessage -> LB ()
doRPL_STATSCONN _msg = return ()

doRPL_LUSERCLIENT :: IrcMessage -> LB ()
doRPL_LUSERCLIENT _msg = return ()

doRPL_LUSEROP :: IrcMessage -> LB ()
doRPL_LUSEROP _msg = return ()

doRPL_LUSERUNKNOWN :: IrcMessage -> LB ()
doRPL_LUSERUNKNOWN _msg = return ()

doRPL_LUSERCHANNELS :: IrcMessage -> LB ()
doRPL_LUSERCHANNELS _msg = return ()

doRPL_LUSERME :: IrcMessage -> LB ()
doRPL_LUSERME _msg = return ()

doRPL_LOCALUSERS :: IrcMessage -> LB ()
doRPL_LOCALUSERS _msg = return ()

doRPL_GLOBALUSERS :: IrcMessage -> LB ()
doRPL_GLOBALUSERS _msg = return ()

doUNKNOWN :: IrcMessage -> Base ()
doUNKNOWN msg
    = debugM $ "UNKNOWN> <" ++ msgPrefix msg ++
      "> [" ++ msgCommand msg ++ "] " ++ show (body msg)

doRPL_NAMREPLY :: IrcMessage -> LB ()
doRPL_NAMREPLY _msg = return ()

doRPL_ENDOFNAMES :: IrcMessage -> LB ()
doRPL_ENDOFNAMES _msg = return ()

doRPL_MOTD :: IrcMessage -> LB ()
doRPL_MOTD _msg = return ()

doRPL_MOTDSTART :: IrcMessage -> LB ()
doRPL_MOTDSTART _msg = return ()

doRPL_ENDOFMOTD :: IrcMessage -> LB ()
doRPL_ENDOFMOTD _msg = return ()
-}

-- Initial output filters

-- | For now, this just checks for duplicate empty lines.
cleanOutput :: Monad m => a -> [String] -> m [String]
cleanOutput _ msg = return $ remDups True msg'
 where
  remDups True ([] : xs) = remDups True xs
  remDups False ([] : xs) = [] : remDups True xs
  remDups _ (x : xs) = x : remDups False xs
  remDups _ [] = []
  msg' = map (dropFromEnd isSpace) msg

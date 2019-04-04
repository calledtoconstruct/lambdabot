-- Copyright (c) 2004 Thomas Jaeger
-- Copyright (c) 2005-6 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)

-- | Keep track of IRC users.
module Lambdabot.Plugin.Social.Seen
  ( seenPlugin
  )
where

import           Lambdabot.Bot
import           Lambdabot.Compat.AltTime
import           Lambdabot.Compat.PackedNick
import           Lambdabot.IRC
import           Lambdabot.Logging
import qualified Lambdabot.Message             as G
import           Lambdabot.Monad
import           Lambdabot.Nick
import           Lambdabot.Plugin
import           Lambdabot.Util

import           Lambdabot.Plugin.Social.Seen.StopWatch
import           Lambdabot.Plugin.Social.Seen.UserStatus

import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans
import           Data.Binary
import qualified Data.ByteString.Char8         as P
import qualified Data.ByteString.Lazy          as L
import           Data.Char
import           Data.List
import qualified Data.Map                      as M
import           Text.Printf

type SeenState = (MaxMap, SeenMap)

type SeenMap = M.Map PackedNick UserStatus

type MaxMap = M.Map Channel Int

type Seen = ModuleT SeenState LB

------------------------------------------------------------------------

seenPlugin :: Module (M.Map Channel Int, M.Map PackedNick UserStatus)
seenPlugin = newModule
  { moduleDefState = return (M.empty, M.empty)
  , moduleCmds     =
    return
      [ (command "users")
        { help    =
          say
            "users [chan]. Report the maximum number of users seen in a channel, and active users in the last 30 minutes"
        , process = doUsers
        }
      , (command "seen")
        { help    = say "seen <user>. Report if a user has been seen by the bot"
        , process = doSeen
        }
      ]
  , moduleInit     =
    do
      sequence_
        [ registerCallback signal (withSeenFM cb)
        | (signal, cb) <- zip
          ["JOIN", "PART", "QUIT", "NICK", "353", "PRIVMSG"]
          [joinCB, partCB, quitCB, nickCB, joinChanCB, msgCB]
        ]
      c <- lb $ findLBFileForReading "seen"
      s <- maybe (return (P.pack "")) (io . P.readFile) c
      let ls = L.fromStrict s
      mbDecoded <- io . try . evaluate $ decode ls
      case mbDecoded of
        Left exc@SomeException{} -> do
          -- try reading the old format (slightly different type... oh, "binary"...)
          mbOld <- io . try . evaluate $ decode ls
          case mbOld of
            Left SomeException{} -> warningM
              ("WARNING: failed to read Seen module state: " ++ show exc)
            Right (maxMap, seenMap) ->
              writeMS (M.mapKeys P.pack maxMap, seenMap)
        Right decoded -> writeMS decoded
  , moduleExit     =
    do
      chans <- lift ircGetChannels
      unless (null chans) $ do
        clockTime <- io getClockTime
        modifyMS $ \(n, m) -> (n, botPart clockTime (map packNick chans) m)
        -- and write out our state:
        withMS $ \s _ ->
          lb (findLBFileForWriting "seen") >>= \c -> io (encodeFile c s)
  }

lcNick :: Nick -> Nick
lcNick (Nick svr nck) = Nick svr (map toLower nck)

------------------------------------------------------------------------

doUsers :: String -> Cmd Seen ()
doUsers rest = withMsg $ \msg -> do
  -- first step towards tracking the maximum number of users
  chan        <- getTarget
  (m, seenFM) <- readMS
  s           <- io getClockTime
  let who = packNick $ lcNick $ if null rest
        then chan
        else parseNick (G.server msg) rest
  let
    now =
      length [ () | (_, Present _ chans) <- M.toList seenFM, who `elem` chans ]
  let n = case M.lookup who m of
        Nothing -> 1
        Just n' -> n'
  let gap_minutes = TimeDiff 1800 -- 30 minutes
  let recent t = diffClockTimes s t < gap_minutes
  let isActive (Present (Just (ct, _td)) _cs) = recent ct
      isActive _                              = False
  let active = length
        [ ()
        | (_, st@(Present _ chans)) <- M.toList seenFM
        , who `elem` chans && isActive st
        ]
  let percent p q = 100 * (fromIntegral p / fromIntegral q) :: Double
  let total 0 0 = "0"
      total p q = printf "%d (%0.1f%%)" p (percent p q)
  say $! printf "Maximum users seen in %s: %d, currently: %s, active: %s"
                (fmtNick (G.server msg) $ unpackNick who)
                n
                (total now n)
                (total active now)

doSeen :: String -> Cmd Seen ()
doSeen rest = withMsg $ \msg -> do
  target      <- getTarget
  (_, seenFM) <- readMS
  now         <- io getClockTime
  let (txt, safe) = (getAnswer msg rest seenFM now)
  if safe || not ("#" `isPrefixOf` nName target)
    then mapM_ say txt
    else lb (ircPrivmsg (G.nick msg) (unlines txt))

replySummary
  :: ClockTime -> M.Map a UserStatus -> (a -> String) -> ([String], Bool)
replySummary now seenFM upAndShow =
  ( [ "Lately, I have seen "
        ++ (if null people
             then "nobody"
             else listToStr "and" (map upAndShow people)
           )
        ++ "."
    ]
  , False
  )
 where
  people = map fst $ filter isActive $ M.toList seenFM
  isActive (_nick, state) = case state of
    (Present (Just (ct, _td)) _cs) -> recent ct
    _                              -> False
  recent t = diffClockTimes now t < gap_minutes
  gap_minutes = TimeDiff 900 -- 15 minutes

replyImHere
  :: Nick
  -> M.Map P.ByteString UserStatus
  -> (Channel -> String)
  -> ([String], Bool)
replyImHere pnick seenFM upAndShow = imHere
 where
  imHere = case M.lookup (packNick pnick) seenFM of
    Just (Present _ cs) ->
      (["Yes, I'm here. I'm in " ++ listToStr "and" (map upAndShow cs)], True)
    _ -> error "I'm here, but not here. And very confused!"

replyCurrent
  :: String -> Nick -> M.Map a UserStatus -> (a -> String) -> ([String], Bool)
replyCurrent nick' pnick seenFM upAndShow =
  ( [ "In "
        ++ nick'
        ++ " I can see "
        ++ (if null people
             then "nobody"
             else listToStr "and" (map upAndShow people)
           )
        ++ "."
    ]
  , False
  )
 where
  people = map fst $ filter inChan $ M.toList seenFM
  inChan (_nick, state) = case state of
    (Present (Just _) cs) -> packNick pnick `elem` cs
    _                     -> False

replySpecific
  :: (G.Message p, Monad m)
  => p
  -> String
  -> Nick
  -> M.Map P.ByteString UserStatus
  -> (Channel -> String)
  -> ClockTime
  -> (m String, Bool)
replySpecific msg nick' pnick seenFM upAndShow now =
  ( return $ concat
    (case M.lookup (packNick pnick) seenFM of
      Just (Present maybeClockTime channels) ->
        nickPresent maybeClockTime (map upAndShow channels)
      Just (NotPresent clockTime td channels) ->
        nickNotPresent clockTime td (map upAndShow channels)
      Just (WasPresent clockTime sw _ channels) ->
        nickWasPresent clockTime sw (map upAndShow channels)
      Just (NewNick newnick) -> nickIsNew newnick
      _                      -> ["I haven't seen ", nick, "."]
    )
  , True
  )
 where
  nickPresent maybeClockTime cs =
    [ if you then "You are" else nick ++ " is"
    , " in "
    , listToStr "and" cs
    , "."
    , case maybeClockTime of
      Nothing -> concat [" I don't know when ", nick, " last spoke."]
      Just (clockTime, missed) -> prettyMissed
        (Stopped missed)
        (concat
          [ " I last heard "
          , nick
          , " speak "
          , lastSpoke {-, ", but "-}
          ]
        )
        (" Last spoke " ++ lastSpoke)
        where lastSpoke = clockDifference clockTime
    ]

  nickNotPresent clockTime missed chans =
    [ "I saw "
    , nick
    , " leaving "
    , listToStr "and" chans
    , " "
    , clockDifference clockTime
    , prettyMissed missed ", and " ""
    ]

  nickWasPresent clockTime sw chans =
    [ "Last time I saw "
    , nick
    , " was when I left "
    , listToStr "and" chans
    , " "
    , clockDifference clockTime
    , prettyMissed sw ", and " ""
    ]

  nickIsNew newnick =
    [if you then "You have" else nick ++ " has", " changed nick to ", us, "."]
      ++ fst (getAnswer msg us seenFM now)
   where
    us = upAndShow $ findFunc newnick
    findFunc pstr = case M.lookup pstr seenFM of
      Just (NewNick pstr') -> findFunc pstr'
      Just _               -> pstr
      Nothing              -> error "SeenModule.nickIsNew: Nothing"
  clockDifference past | all (== ' ') diff = "just now"
                       | otherwise         = diff ++ " ago"
    where diff = timeDiffPretty . diffClockTimes now $ past
  you  = pnick == lcNick (G.nick msg)
  nick = if you then "you" else nick'

  prettyMissed (Stopped _) _ifMissed _            = "." -- ifMissed ++ "."
  prettyMissed _           _         _ifNotMissed = "." -- ifNotMissed ++ "."

        {-
                prettyMissed (Stopped missed) ifMissed _
                    | missedPretty <- timeDiffPretty missed
                    , any (/=' ') missedPretty
                    = concat [ifMissed, "I have missed ", missedPretty, " since then."]
        
                prettyMissed _ _ ifNotMissed = ifNotMissed ++ "."
        -}

getAnswer
  :: G.Message a => a -> String -> SeenMap -> ClockTime -> ([String], Bool)
getAnswer msg rest seenFM now
  | null nick'                   = replySummary now seenFM upAndShow
  | pnick == G.lambdabotName msg = replyImHere pnick seenFM upAndShow
  | head (nName pnick) == '#'    = replyCurrent nick' pnick seenFM upAndShow
  | otherwise = replySpecific msg nick' pnick seenFM upAndShow now
 where
  upAndShow = fmtNick (G.server msg) . unpackNick
  nick'     = takeWhile (not . isSpace) rest
  pnick     = lcNick $ parseNick (G.server msg) nick'

-- | extract channels from message as packed, lower cased, strings.
msgChans :: G.Message a => a -> [Channel]
msgChans = map (packNick . lcNick) . G.channels

-- | Callback for when somebody joins. If it is not the bot that joins, record
--   that we have a new user in our state tree and that we have never seen the
--   user speaking.
joinCB
  :: IrcMessage -> ClockTime -> PackedNick -> SeenMap -> Either String SeenMap
joinCB msg _ct nick fm
  | nick == lbNick = Right fm
  | otherwise      = Right $! insertUpd (updateJ Nothing chans) nick newInfo fm
 where
  insertUpd f = M.insertWith (\_ -> f)
  lbNick  = packNick $ G.lambdabotName msg
  newInfo = Present Nothing chans
  chans   = msgChans msg

-- | Update the state to reflect the bot leaving channel(s)
botPart :: ClockTime -> [Channel] -> SeenMap -> SeenMap
botPart ct cs = fmap botPart'
 where
  botPart' (Present mct xs) = case xs \\ cs of
    [] -> WasPresent ct (startWatch ct zeroWatch) mct cs
    ys -> Present mct ys
  botPart' (NotPresent ct' missed c) | head c `elem` cs =
    NotPresent ct' (startWatch ct missed) c
  botPart' (WasPresent ct' missed mct c) | head c `elem` cs =
    WasPresent ct' (startWatch ct missed) mct c
  botPart' us = us

-- | when somebody parts
partCB
  :: IrcMessage -> ClockTime -> PackedNick -> SeenMap -> Either String SeenMap
partCB msg ct nick fm
  | nick == lbNick = Right $ botPart ct (msgChans msg) fm
  | otherwise = case M.lookup nick fm of
    Just (Present mct xs) -> case xs \\ (msgChans msg) of
      [] -> Right $! M.insert nick (NotPresent ct zeroWatch xs) fm
      ys -> Right $! M.insert nick (Present mct ys) fm
    _ -> Left "someone who isn't known parted"
  where lbNick = packNick $ G.lambdabotName msg

-- | when somebody quits
quitCB
  :: IrcMessage -> ClockTime -> PackedNick -> SeenMap -> Either String SeenMap
quitCB _ ct nick fm = case M.lookup nick fm of
  Just (Present _ct xs) ->
    Right $! M.insert nick (NotPresent ct zeroWatch xs) fm
  _ -> Left "someone who isn't known has quit"

-- | when somebody changes his\/her name
nickCB
  :: IrcMessage -> ClockTime -> PackedNick -> SeenMap -> Either String SeenMap
nickCB msg _ nick fm = case M.lookup nick fm of
  Just status ->
    Right $! M.insert lcnewnick status $ M.insert nick (NewNick lcnewnick) fm
  _ -> Left "someone who isn't here changed nick"
 where
  newnick   = drop 1 $ head (ircMsgParams msg)
  lcnewnick = packNick $ lcNick $ parseNick (G.server msg) newnick

-- | when the bot joins a channel
joinChanCB
  :: IrcMessage -> ClockTime -> PackedNick -> SeenMap -> Either String SeenMap
joinChanCB msg now _nick fm = Right
  $! fmap (updateNP now chan) (foldl insertNick fm chanUsers)
 where
  l    = ircMsgParams msg
  chan = packNick $ lcNick $ parseNick (G.server msg) $ l !! 2
  chanUsers =
    map (packNick . lcNick . parseNick (G.server msg)) $ words (drop 1 (l !! 3)) -- remove ':'
  unUserMode nick = Nick (nTag nick) (dropWhile (`elem` "@+") $ nName nick)
  insertUpd f = M.insertWith (const f)
  insertNick fm' u = insertUpd
    (updateJ (Just now) [chan])
    (packNick . unUserMode . lcNick . unpackNick $ u)
    (Present Nothing [chan])
    fm'

-- | when somebody speaks, update their clocktime
msgCB
  :: IrcMessage -> ClockTime -> PackedNick -> SeenMap -> Either String SeenMap
msgCB msg clockTime nick fm = do
  let channel  = packNick . lcNick . head . G.channels $! msg
  let presence = M.lookup nick fm
  case presence of
    Just (Present _ xs) ->
      Right $! M.insert nick (Present (Just (clockTime, noTimeDiff)) xs) fm
    _ ->
      Right
        $! M.insert nick (Present (Just (clockTime, noTimeDiff)) [channel]) fm

-- | Callbacks are only allowed to use a limited knowledge of the world.
-- 'withSeenFM' is (up to trivial isomorphism) a monad morphism from the
-- restricted
--   'ReaderT (IRC.Message, ClockTime, Nick) (StateT SeenState (Error String))'
-- to the
--   'ReaderT IRC.Message (Seen IRC)'
-- monad.
withSeenFM
  :: G.Message a
  => (a -> ClockTime -> PackedNick -> SeenMap -> Either String SeenMap)
  -> (a -> Seen ())
withSeenFM f msg = do
  let channel  = lcNick . head . G.channels $! msg
  let nickname = lcNick . G.nick $ msg
  noticeM
    $  "Received call back "
    ++ nTag channel
    ++ " <> "
    ++ nName channel
    ++ " <> "
    ++ nTag nickname
    ++ " <> "
    ++ nName nickname
  let packedChannel  = packNick channel
  let packedNickname = packNick nickname
  withMS $ \(maxUsers, state) writer -> do
    clockTime <- io getClockTime
    case f msg clockTime packedNickname state of
      Left  _        -> return ()
      Right newstate -> do
        let curUsers =
              length
                $! [ ()
                   | (_, Present _ chans) <- M.toList state
                   , packedChannel `elem` chans
                   ]
        let newMax = case M.lookup packedChannel maxUsers of
              Nothing -> M.insert packedChannel curUsers maxUsers
              Just n  -> if n < curUsers
                then M.insert packedChannel curUsers maxUsers
                else maxUsers
        newMax `seq` newstate `seq` writer (newMax, newstate)

-- Copyright (c) 2004 Thomas Jaeger
-- Copyright (c) 2005 Simon Winwood
-- Copyright (c) 2005 Don Stewart
-- Copyright (c) 2005 David House <dmouse@gmail.com>
--

-- | Logging an IRC channel..
module Lambdabot.Plugin.IRC.Log (
  logPlugin,
) where

import Lambdabot.Compat.FreenodeNick (FreenodeNick (FreenodeNick))
import Lambdabot.IRC (IrcMessage (ircMsgParams))
import qualified Lambdabot.Message as Msg
import Lambdabot.Monad (MonadLB (lb), registerCallback)
import Lambdabot.Nick (Nick (nName, nTag), parseNick)
import Lambdabot.Plugin (
  LB,
  Module (moduleDefState, moduleExit, moduleInit),
  ModuleT,
  MonadLBState (withMS),
  findLBFileForWriting,
  modifyMS,
  newModule,
  readMS,
 )
import Lambdabot.Util (io)

import Control.Monad (unless, when)
import qualified Data.Map as M
import Data.Time (UTCTime (UTCTime), getCurrentTime, toGregorian)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((<.>), (</>))
import System.IO (
  Handle,
  IOMode (AppendMode),
  hClose,
  hFlush,
  hPutStrLn,
  openFile,
 )

-- ------------------------------------------------------------------------

type Channel = Nick

type DateStamp = (Int, Int, Integer)

data ChanState = CS
  { chanHandle :: Handle
  , chanDate :: DateStamp
  }
  deriving (Show, Eq)

type LogState = M.Map Channel ChanState

type Log = ModuleT LogState LB

data Event
  = Said Nick UTCTime String
  | Joined Nick String UTCTime
  | Parted Nick String UTCTime -- covers quitting as well
  | Kicked Nick Nick String UTCTime String
  | Renick Nick String UTCTime Nick
  | Mode Nick String UTCTime String
  deriving (Eq)

instance Show Event where
  show (Said nick ct what) = timeStamp ct ++ " <" ++ nName nick ++ "> " ++ what
  show (Joined nick usr ct) = timeStamp ct ++ " " ++ show (FreenodeNick nick) ++ " (" ++ usr ++ ") joined."
  show (Parted nick usr ct) = timeStamp ct ++ " " ++ show (FreenodeNick nick) ++ " (" ++ usr ++ ") left."
  show (Kicked nick op usrop ct reason) = timeStamp ct ++ " " ++ show (FreenodeNick nick) ++ " was kicked by " ++ show (FreenodeNick op) ++ " (" ++ usrop ++ "): " ++ reason ++ "."
  show (Renick nick usr ct new) = timeStamp ct ++ " " ++ show (FreenodeNick nick) ++ " (" ++ usr ++ ") is now " ++ show (FreenodeNick new) ++ "."
  show (Mode nick usr ct mode) = timeStamp ct ++ " " ++ show (FreenodeNick nick) ++ " (" ++ usr ++ ") changed mode to " ++ mode ++ "."

doLog :: (IrcMessage -> UTCTime -> Event) -> IrcMessage -> (Handle -> UTCTime -> Log ())
doLog f m hdl = logString hdl . show . f m

connect :: String -> (IrcMessage -> UTCTime -> Event) -> ModuleT LogState LB ()
connect signal callback = registerCallback signal $ \msg -> do
  now <- io getCurrentTime
  -- map over the channels this message was directed to, adding to each
  -- of their log files.
  mapM_ (withValidLog (doLog callback msg) now) (Msg.channels msg)
-- * Dispatchers and Module instance declaration

--
logPlugin :: Module (M.Map Channel ChanState)
logPlugin =
  newModule
    { moduleDefState = return M.empty
    , moduleExit = cleanLogState
    , moduleInit = do
        connect "PRIVMSG" msgCB
        connect "JOIN" joinCB
        connect "PART" partCB
        connect "KICK" kickCB
        connect "NICK" nickCB
        connect "MODE" modeCB
    }

-- * Logging helpers

--

-- | Show a number, padded to the left with zeroes up to the specified width
showWidth :: Int -> Int -> String
showWidth width n = zeroes ++ num
 where
  num = show n
  zeroes = replicate (width - length num) '0'

timeStamp :: UTCTime -> String
timeStamp (UTCTime _ ct) = showWidth 2 (hours `mod` 24) ++ ":" ++ showWidth 2 (mins `mod` 60) ++ ":" ++ showWidth 2 (secs `mod` 60)
 where
  secs = round ct :: Int
  mins = secs `div` 60
  hours = mins `div` 60

-- | Show a DateStamp.
dateToString :: DateStamp -> String
dateToString (d, m, y) = showWidth 2 (fromInteger y) ++ "-" ++ showWidth 2 (fromEnum m + 1) ++ "-" ++ showWidth 2 d

-- | UTCTime -> DateStamp conversion
dateStamp :: UTCTime -> DateStamp
dateStamp (UTCTime day _) = (d, m, y)
 where
  (y, m, d) = toGregorian day

-- * State manipulation functions

--

-- | Cleans up after the module (closes files)
cleanLogState :: Log ()
cleanLogState = withMS $ \state writer -> do
  io $ M.foldr (\cs iom -> iom >> hClose (chanHandle cs)) (return ()) state
  writer M.empty

-- | Fetch a channel from the internal map. Uses LB's fail if not found.
getChannel :: Channel -> Log ChanState
getChannel = (readMS >>=) . mLookup
 where
  mLookup k = maybe (fail "getChannel: not found") return . M.lookup k

getDate :: Channel -> Log DateStamp
getDate = fmap chanDate . getChannel

getHandle :: Channel -> Log Handle
getHandle = fmap chanHandle . getChannel

-- add points. otherwise:
-- Unbound implicit parameters (?ref::GHC.IOBase.MVar LogState, ?name::String)
--  arising from instantiating a type signature at
-- Plugin/Log.hs:187:30-39
-- Probable cause: `getChannel' is applied to too few arguments

{- | Put a DateStamp and a Handle. Used by 'openChannelFile' and
  'reopenChannelMaybe'.
-}
putHdlAndDS :: Channel -> Handle -> DateStamp -> Log ()
putHdlAndDS c hdl ds = modifyMS (M.adjust (\cs -> cs{chanHandle = hdl, chanDate = ds}) c)

-- * Logging IO

--

-- | Open a file to write the log to.
openChannelFile :: Channel -> UTCTime -> Log Handle
openChannelFile chan ct = do
  logDir <- lb $ findLBFileForWriting "Log"
  let dir = logDir </> nTag chan </> nName chan
  let file = dir </> dateToString date <.> "txt"
  io $ createDirectoryIfMissing True dir >> openFile file AppendMode
 where
  date = dateStamp ct

-- | Close and re-open a log file, and update the state.
reopenChannelMaybe :: Channel -> UTCTime -> Log ()
reopenChannelMaybe chan ct = do
  date <- getDate chan
  when (date /= dateStamp ct) $ do
    hdl <- getHandle chan
    io $ hClose hdl
    hdl' <- openChannelFile chan ct
    putHdlAndDS chan hdl' (dateStamp ct)

-- | Initialise the channel state (if it not already inited)
initChannelMaybe :: Nick -> UTCTime -> Log ()
initChannelMaybe chan ct = do
  chanp <- fmap (M.member chan) readMS
  unless chanp $ do
    hdl <- openChannelFile chan ct
    modifyMS (M.insert chan $ CS hdl (dateStamp ct))

-- | Ensure that the log is correctly initialised etc.
withValidLog :: (Handle -> UTCTime -> Log a) -> UTCTime -> Channel -> Log a
withValidLog f clockTime chan = do
  initChannelMaybe chan clockTime
  reopenChannelMaybe chan clockTime
  hdl <- getHandle chan
  f hdl clockTime

-- | Log a string. Main logging workhorse.
logString :: Handle -> String -> Log ()
logString hdl str = io $ hPutStrLn hdl str >> hFlush hdl

-- We flush on each operation to ensure logs are up to date.

-- * The event loggers themselves

--

-- | When somebody joins.
joinCB :: IrcMessage -> UTCTime -> Event
joinCB msg = Joined (Msg.nick msg) (Msg.fullName msg)

-- | When somebody quits.
partCB :: IrcMessage -> UTCTime -> Event
partCB msg = Parted (Msg.nick msg) (Msg.fullName msg)

-- | When somebody is kicked.
kickCB :: IrcMessage -> UTCTime -> Event
kickCB msg ct =
  Kicked
    (Msg.nick msg){nName = head $ tail $ ircMsgParams msg}
    (Msg.nick msg)
    (Msg.fullName msg)
    ct
    (tail . concat . tail . tail $ ircMsgParams msg)

{- | When somebody changes his\/her name.
 TODO:  We should only do this for channels that the user is currently on.
-}
nickCB :: IrcMessage -> UTCTime -> Event
nickCB msg ct =
  Renick
    (Msg.nick msg)
    (Msg.fullName msg)
    ct
    (parseNick (Msg.server msg) $ drop 1 $ head $ ircMsgParams msg)

-- | When somebody changes channel mode.
modeCB :: IrcMessage -> UTCTime -> Event
modeCB msg ct =
  Mode
    (Msg.nick msg)
    (Msg.fullName msg)
    ct
    (unwords $ tail $ ircMsgParams msg)

-- | When somebody speaks.
msgCB :: IrcMessage -> UTCTime -> Event
msgCB msg ct =
  Said
    (Msg.nick msg)
    ct
    (tail . concat . tail $ ircMsgParams msg)

-- each lines is :foo

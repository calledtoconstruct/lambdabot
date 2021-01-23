{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Lambdabot.Bot (
  ircLoadModule,
  ircUnloadModule,
  checkPrivs,
  checkIgnore,
  ircCodepage,
  ircGetChannels,
  ircQuit,
  ircReconnect,
  ircPrivmsg,
  ircPrivmsg',
) where

import Control.Concurrent (threadDelay)
import Control.Exception.Lifted as E (SomeException (SomeException), catch)
import Control.Monad.Reader (MonadIO (liftIO), asks, liftM2, when)
import Control.Monad.State (gets, modify)
import qualified Data.Map as M
import Data.Random.Source (MonadRandom (..), monadRandom)
import qualified Data.Set as S
import Lambdabot.ChanName (getCN)
import Lambdabot.Config (MonadConfig (getConfig))
import Lambdabot.Config.Core (textWidth)
import Lambdabot.IRC (IrcMessage, codepage, privmsg, quit)
import Lambdabot.Logging (errorM, infoM, noticeM)
import Lambdabot.Message (Message (nick))
import Lambdabot.Module (
  IRCRWState (ircChannels, ircIgnoredUsers, ircPersists, ircPrivilegedUsers),
  LB,
  Module (moduleCmds, moduleDefState, moduleExit, moduleInit, moduleSticky),
  ModuleInfo (theModule),
  runModuleT,
 )
import Lambdabot.Monad (applyOutputFilters, inModuleNamed, registerCommands, registerModule, send, unregisterModule)
import Lambdabot.Nick (Nick)
import Lambdabot.State (readGlobalState, writeGlobalState)

------------------------------------------------------------------------
--

-- | Register a module in the irc state
ircLoadModule :: String -> Module st -> LB ()
ircLoadModule moduleName moduleToLoad = do
  infoM ("Loading module " ++ show moduleName)
  savedState <- readGlobalState moduleToLoad moduleName
  mState <- maybe (moduleDefState moduleToLoad) return savedState
  mInfo <- registerModule moduleName moduleToLoad mState
  runModuleT (do moduleInit moduleToLoad; registerCommands =<< moduleCmds moduleToLoad) mInfo
    `E.catch` \e@SomeException{} -> do
      errorM ("Module " ++ show moduleName ++ " failed to load.  Exception thrown: " ++ show e)
      unregisterModule moduleName
      fail "Refusing to load due to a broken plugin"

--

-- | Unregister a module's entry in the irc state
ircUnloadModule :: String -> LB ()
ircUnloadModule moduleName = do
  infoM ("Unloading module " ++ show moduleName)
  inModuleNamed moduleName (fail "module not loaded") $ do
    moduleToUnload <- asks theModule
    when (moduleSticky moduleToUnload) $ fail "module is sticky"
    moduleExit moduleToUnload
      `E.catch` \e@SomeException{} -> errorM ("Module " ++ show moduleName ++ " threw the following exception in moduleExit: " ++ show e)
    writeGlobalState
  unregisterModule moduleName

------------------------------------------------------------------------

-- | Checks whether the given user has admin permissions
checkPrivs :: IrcMessage -> LB Bool
checkPrivs msg = gets (S.member (nick msg) . ircPrivilegedUsers)

{- | Checks whether the given user is being ignored.
   Privileged users can't be ignored.
-}
checkIgnore :: IrcMessage -> LB Bool
checkIgnore msg = liftM2 (&&) (fmap not (checkPrivs msg)) (gets (S.member (nick msg) . ircIgnoredUsers))

------------------------------------------------------------------------
-- Some generic server operations

-- Send a CODEPAGE command to set encoding for current session.
-- Some IRC networks don't provide UTF-8 ports, but allow
-- switching it in runtime
ircCodepage :: String -> String -> LB ()
ircCodepage svr cpage = send $ codepage svr cpage

ircGetChannels :: LB [Nick]
ircGetChannels = gets ((map getCN . M.keys) . ircChannels)

-- Send a quit message, settle and wait for the server to drop our
-- handle. At which point the main thread gets a closed handle eof
-- exceptoin, we clean up and go home
ircQuit :: String -> String -> LB ()
ircQuit svr msg = do
  modify $ \state' -> state'{ircPersists = M.delete svr $ ircPersists state'}
  send $ quit svr msg
  liftIO $ threadDelay 1000
  noticeM "Quitting"

ircReconnect :: String -> String -> LB ()
ircReconnect svr msg = do
  modify $ \state' ->
    state'
      { ircPersists = M.insertWith (\_ x -> x) svr False $ ircPersists state'
      }
  send $ quit svr msg
  liftIO $ threadDelay 1000

-- | Send a message to a channel\/user, applying all output filters
ircPrivmsg :: Nick -> String -> LB ()
ircPrivmsg who msg = do
  sendlines <- applyOutputFilters who msg
  w <- getConfig textWidth
  mapM_ (ircPrivmsg' who . take w) $ take 10 sendlines

-- A raw send version (bypasses output filters)
ircPrivmsg' :: Nick -> String -> LB ()
ircPrivmsg' who "" = ircPrivmsg' who " "
ircPrivmsg' who msg = send $ privmsg who msg

------------------------------------------------------------------------

monadRandom
  [d|
    instance MonadRandom LB where
      getRandomWord8 = liftIO getRandomWord8
      getRandomWord16 = liftIO getRandomWord16
      getRandomWord32 = liftIO getRandomWord32
      getRandomWord64 = liftIO getRandomWord64
      getRandomDouble = liftIO getRandomDouble
      getRandomNByteInteger n = liftIO (getRandomNByteInteger n)
    |]

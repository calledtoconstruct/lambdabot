
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Lambdabot.Bot
  ( ircLoadModule
  , ircUnloadModule
  , checkPrivs
  , checkIgnore
  , ircCodepage
  , ircGetChannels
  , ircQuit
  , ircReconnect
  , ircPrivmsg
  , ircPrivmsg'
  )
where

import           Lambdabot.ChanName
import           Lambdabot.Config
import           Lambdabot.Config.Core
import           Lambdabot.IRC
import           Lambdabot.Logging
import           Lambdabot.Message
import           Lambdabot.Module
import           Lambdabot.Monad
import           Lambdabot.Nick
import           Lambdabot.State

import           Control.Concurrent
import           Control.Exception.Lifted      as E
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.Map                      as M
import           Data.Random.Source
import qualified Data.Set                      as S

------------------------------------------------------------------------
--
-- | Register a module in the irc state
--
ircLoadModule :: String -> Module st -> LB ()
ircLoadModule mName m = do
  infoM ("Loading module " ++ show mName)

  savedState <- readGlobalState m mName
  mState     <- maybe (moduleDefState m) return savedState

  mInfo      <- registerModule mName m mState

  runModuleT
      (do
        moduleInit m
        registerCommands =<< moduleCmds m
      )
      mInfo
    `E.catch` \e@SomeException{} -> do
                errorM
                  (  "Module "
                  ++ show mName
                  ++ " failed to load.  Exception thrown: "
                  ++ show e
                  )
                unregisterModule mName
                fail "Refusing to load due to a broken plugin"

--
-- | Unregister a module's entry in the irc state
--
ircUnloadModule :: String -> LB ()
ircUnloadModule mName = do
  infoM ("Unloading module " ++ show mName)

  inModuleNamed mName (fail "module not loaded") $ do
    m <- asks theModule
    when (moduleSticky m) $ fail "module is sticky"

    moduleExit m `E.catch` \e@SomeException{} -> errorM
      (  "Module "
      ++ show mName
      ++ " threw the following exception in moduleExit: "
      ++ show e
      )

    writeGlobalState

  unregisterModule mName

------------------------------------------------------------------------

-- | Checks whether the given user has admin permissions
checkPrivs :: IrcMessage -> LB Bool
checkPrivs msg = gets (S.member (nick msg) . ircPrivilegedUsers)

-- | Checks whether the given user is being ignored.
--   Privileged users can't be ignored.
checkIgnore :: IrcMessage -> LB Bool
checkIgnore msg = liftM2 (&&)
                         (fmap not (checkPrivs msg))
                         (gets (S.member (nick msg) . ircIgnoredUsers))

------------------------------------------------------------------------
-- Some generic server operations

-- Send a CODEPAGE command to set encoding for current session.
-- Some IRC networks don't provide UTF-8 ports, but allow
-- switching it in runtime
ircCodepage :: String -> String -> LB ()
ircCodepage svr cpage = send $ codepage svr cpage

ircGetChannels :: LB [Nick]
ircGetChannels = (map getCN . M.keys) `fmap` gets ircChannels

-- Send a quit message, settle and wait for the server to drop our
-- handle. At which point the main thread gets a closed handle eof
-- exceptoin, we clean up and go home
ircQuit :: String -> String -> LB ()
ircQuit svr msg = do
  modify $ \state' -> state' { ircPersists = M.delete svr $ ircPersists state' }
  send $ quit svr msg
  liftIO $ threadDelay 1000
  noticeM "Quitting"

ircReconnect :: String -> String -> LB ()
ircReconnect svr msg = do
  modify $ \state' -> state'
    { ircPersists = M.insertWith (flip const) svr False $ ircPersists state'
    }
  send $ quit svr msg
  liftIO $ threadDelay 1000

-- | Send a message to a channel\/user, applying all output filters
ircPrivmsg :: Nick -> String -> LB ()
ircPrivmsg who msg = do
  sendlines <- applyOutputFilters who msg
  w         <- getConfig textWidth
  mapM_ (ircPrivmsg' who . take w) $ take 10 sendlines

-- A raw send version (bypasses output filters)
ircPrivmsg' :: Nick -> String -> LB ()
ircPrivmsg' who ""  = ircPrivmsg' who " "
ircPrivmsg' who msg = send $ privmsg who msg

------------------------------------------------------------------------

monadRandom [d|
    instance MonadRandom LB where
        getRandomWord8          = liftIO getRandomWord8
        getRandomWord16         = liftIO getRandomWord16
        getRandomWord32         = liftIO getRandomWord32
        getRandomWord64         = liftIO getRandomWord64
        getRandomDouble         = liftIO getRandomDouble
        getRandomNByteInteger n = liftIO (getRandomNByteInteger n)
 |]

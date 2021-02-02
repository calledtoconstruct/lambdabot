{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Lambdabot.Monad (
  IRCRState,
  initRoState,
  reportInitDone,
  waitForInit,
  waitForQuit,
  Callback,
  OutputFilter,
  Server,
  IRCRWState (..),
  initRwState,
  runLB,
  MonadLB (..),
  registerModule,
  registerCommands,
  registerCallback,
  registerOutputFilter,
  unregisterModule,
  registerServer,
  unregisterServer,
  send,
  received,
  applyOutputFilters,
  inModuleNamed,
  inModuleWithID,
  withCommand,
  listModules,
  withAllModules,
) where

import Lambdabot.Command (Cmd, Command, cmdNames)
import Lambdabot.Config (Config, MonadConfig (..), getConfigDefault, mergeConfig)
import Lambdabot.Config.Core (lbRootLoggerPath, uncaughtExceptionHandler)
import Lambdabot.IRC (IrcMessage (ircMsgCommand))
import Lambdabot.Logging (MonadLogging (..), infoM, warningM)
import qualified Lambdabot.Message as Msg
import Lambdabot.Module (
  Callback,
  CallbackRef (CallbackRef),
  CommandRef (CommandRef),
  IRCRState (..),
  IRCRWState (..),
  LB (..),
  Module,
  ModuleID,
  ModuleInfo (ModuleInfo, moduleID),
  ModuleT,
  OutputFilter,
  OutputFilterRef (..),
  Server,
  ServerRef (ServerRef),
  newModuleID,
  runModuleT,
 )
import Lambdabot.Nick (Nick)
import Lambdabot.Util (io)

import Control.Concurrent.Lifted (newEmptyMVar, newMVar, putMVar, readMVar)
import Control.Exception.Lifted as E (catch)
import Control.Monad.Base (MonadBase (..))
import Control.Monad.Identity (Identity (..), forM_, when, (<=<))
import Control.Monad.Reader (MonadIO, MonadTrans (lift), ReaderT (runReaderT), asks)
import Control.Monad.State (MonadState (..), gets, modify)
import Control.Monad.Trans.Control (MonadBaseControl (..))
import qualified Data.Dependent.Map as D
import Data.Dependent.Sum (DSum ((:=>)))
import Data.IORef (IORef, atomicModifyIORef)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Some (Some (Some))

-- | Default ro state
initRoState :: [DSum Config Identity] -> IO IRCRState
initRoState configuration = do
  quitMVar <- newEmptyMVar
  initDoneMVar <- newEmptyMVar

  let mergeConfig' k (Identity x) (Identity y) = Identity (mergeConfig k y x)

  return
    IRCRState
      { ircQuitMVar = quitMVar
      , ircInitDoneMVar = initDoneMVar
      , ircConfig = D.fromListWithKey mergeConfig' configuration
      }

reportInitDone :: LB ()
reportInitDone = do
  mvar <- LB (asks (ircInitDoneMVar . fst))
  io $ putMVar mvar ()

askLB :: MonadLB m => (IRCRState -> a) -> m a
askLB f = lb . LB $ asks (f . fst)

waitForInit :: MonadLB m => m ()
waitForInit = readMVar =<< askLB ircInitDoneMVar

waitForQuit :: MonadLB m => m ()
waitForQuit = readMVar =<< askLB ircQuitMVar

-- | Default rw state
initRwState :: IRCRWState
initRwState =
  IRCRWState
    { ircPrivilegedUsers = S.empty
    , ircIgnoredUsers = S.empty
    , ircChannels = M.empty
    , ircPersists = M.empty
    , ircModulesByName = M.empty
    , ircModulesByID = D.empty
    , ircServerMap = M.empty
    , ircCallbacks = M.empty
    , ircOutputFilters = []
    , ircCommands = M.empty
    }

runLB :: LB a -> (IRCRState, IORef IRCRWState) -> IO a
runLB = runReaderT . unLB

instance MonadBase IO LB where
  liftBase = LB . liftBase

instance MonadBaseControl IO LB where
  type StM LB a = StM (ReaderT (IRCRState, IORef IRCRWState) IO) a
  liftBaseWith action = LB (liftBaseWith (\run -> action (run . unLB)))
  restoreM = LB . restoreM

class (MonadIO m, MonadBaseControl IO m, MonadConfig m, MonadLogging m, Applicative m, MonadFail m) => MonadLB m where
  lb :: LB a -> m a

instance MonadLB LB where
  lb = id
instance MonadLB m => MonadLB (ModuleT st m) where
  lb = lift . lb
instance MonadLB m => MonadLB (Cmd m) where
  lb = lift . lb

instance MonadState IRCRWState LB where
  state f = LB $ do
    ref <- asks snd
    lift . atomicModifyIORef ref $ \s -> let (s', x) = f s in seq s' (x, s')

instance MonadConfig LB where
  getConfig k =
    fmap
      (maybe (getConfigDefault k) runIdentity . D.lookup k)
      (lb (askLB ircConfig))

instance MonadLogging LB where
  getCurrentLogger = getConfig lbRootLoggerPath
  logM a b c = io (logM a b c)

---------------
-- state management (registering/unregistering various things)

registerModule :: String -> Module st -> st -> LB (ModuleInfo st)
registerModule mName m mState = do
  mTag <- io newModuleID
  mInfo <- ModuleInfo mName mTag m <$> newMVar mState

  modify $ \s ->
    s
      { ircModulesByName = M.insert mName (Some mInfo) (ircModulesByName s)
      , ircModulesByID = D.insert mTag mInfo (ircModulesByID s)
      }

  return mInfo

registerCommands :: [Command (ModuleT st LB)] -> ModuleT st LB ()
registerCommands cmds = do
  mTag <- asks moduleID
  let taggedCmds =
        [ (cName, mTag :=> CommandRef cmd)
        | cmd <- cmds
        , cName <- cmdNames cmd
        ]

  lift $
    modify $ \s ->
      s{ircCommands = M.union (M.fromList taggedCmds) (ircCommands s)}

registerCallback :: String -> Callback st -> ModuleT st LB ()
registerCallback str f = do
  mTag <- asks moduleID
  lift . modify $ \s -> s{ircCallbacks = M.insertWith D.union str (D.singleton mTag (CallbackRef f)) (ircCallbacks s)}

registerOutputFilter :: OutputFilter st -> ModuleT st LB ()
registerOutputFilter f = do
  mTag <- asks moduleID
  lift . modify $ \s ->
    s{ircOutputFilters = (mTag :=> OutputFilterRef f) : ircOutputFilters s}

unregisterModule :: String -> LB ()
unregisterModule mName = maybe (return ()) warningM <=< state $ \s ->
  case M.lookup mName (ircModulesByName s) of
    Nothing ->
      ( Just $
          "Tried to unregister module that wasn't registered: "
            ++ show mName
      , s
      )
    Just (Some modInfo) ->
      let mTag = moduleID modInfo

          notThisTag :: DSum ModuleID f -> Bool
          notThisTag (tag :=> _) = Some tag /= Some mTag
          s' =
            s
              { ircModulesByName = M.delete mName (ircModulesByName s)
              , ircModulesByID = D.delete mTag (ircModulesByID s)
              , ircCommands = M.filter notThisTag (ircCommands s)
              , ircCallbacks = M.map (D.delete mTag) (ircCallbacks s)
              , ircServerMap = M.filter notThisTag (ircServerMap s)
              , ircOutputFilters = filter notThisTag (ircOutputFilters s)
              }
       in (Nothing, s')

-- The virtual chat system.
--
-- The virtual chat system sits between the chat drivers and the rest of
-- Lambdabot.  It provides a mapping between the String server "tags" and
-- functions which are able to handle sending messages.
--
-- When a message is received, the chat module is expected to call
-- `Lambdabot.Main.received'.  This is not ideal.

registerServer :: String -> Server st -> ModuleT st LB ()
registerServer sName sendf = do
  mTag <- asks moduleID
  maybe (return ()) fail <=< lb . state $ \s ->
    case M.lookup sName (ircServerMap s) of
      Just _ -> (Just $ "attempted to create two servers named " ++ sName, s)
      Nothing ->
        let s' = s{ircServerMap = M.insert sName (mTag :=> ServerRef sendf) (ircServerMap s)}
         in (Nothing, s')

-- TODO: fix race condition
unregisterServer :: String -> ModuleT mod LB ()
unregisterServer tag = lb $ do
  infoM $ "unregistering server " ++ tag
  s <- get
  let svrs = ircServerMap s
  case M.lookup tag svrs of
    Just _ -> do
      let svrs' = M.delete tag svrs
      put (s{ircServerMap = svrs'})
      infoM "server unregistered"
      when (M.null svrs') $ do
        infoM "all servers unregistered"
        quitMVar <- askLB ircQuitMVar
        io $ putMVar quitMVar ()
    Nothing -> fail $ "attempted to delete nonexistent servers named " ++ tag

withUEHandler :: LB () -> LB ()
withUEHandler f = do
  handler <- getConfig uncaughtExceptionHandler
  E.catch f (io . handler)

send :: IrcMessage -> LB ()
send msg = do
  s <- gets ircServerMap
  let bogusServer = warningM $ "sending message to bogus server: " ++ show msg
  let maybeServerName = M.lookup (Msg.server msg) s
  maybe bogusServer (\(mTag :=> ServerRef sendf) -> withUEHandler $ inModuleWithID mTag bogusServer $ sendf msg) maybeServerName

received :: IrcMessage -> LB ()
received msg = do
  ircState <- get
  let maybeCallback = M.lookup (ircMsgCommand msg) (ircCallbacks ircState)
  let doNothing = return ()
  let doCallback = \(tag :=> CallbackRef cb) -> withUEHandler (inModuleWithID tag doNothing (cb msg))
  let doCallbacks = \cbs -> forM_ (D.toList cbs) doCallback
  maybe doNothing doCallbacks maybeCallback

applyOutputFilter :: Nick -> DSum ModuleID OutputFilterRef -> [String] -> LB [String]
applyOutputFilter who (mTag :=> OutputFilterRef f) msg =
  inModuleWithID mTag (return msg) (f who msg)

applyOutputFilters :: Nick -> String -> LB [String]
applyOutputFilters who msg = do
  filters <- gets ircOutputFilters
  foldr (\a x -> applyOutputFilter who a =<< x) ((return . lines) msg) filters

------------------------------------------------------------------------
-- Module handling

-- | Interpret an expression in the context of a module.
inModuleNamed :: String -> LB a -> (forall st. ModuleT st LB a) -> LB a
inModuleNamed name nothing just = do
  mbMod <- gets (M.lookup name . ircModulesByName)
  maybe nothing (\(Some modInfo) -> runModuleT just modInfo) mbMod

inModuleWithID :: ModuleID st -> LB a -> ModuleT st LB a -> LB a
inModuleWithID tag nothing just = do
  mbMod <- gets (D.lookup tag . ircModulesByID)
  maybe nothing (runModuleT just) mbMod

withCommand :: String -> LB a -> (forall st. Command (ModuleT st LB) -> ModuleT st LB a) -> LB a
withCommand cmdname def f = do
  mbCmd <- gets (M.lookup cmdname . ircCommands)
  maybe def (\(tag :=> CommandRef cmd) -> inModuleWithID tag def $ f cmd) mbCmd

listModules :: LB [String]
listModules = gets (M.keys . ircModulesByName)

-- | Interpret a function in the context of all modules
withAllModules :: (forall st. ModuleT st LB a) -> LB ()
withAllModules f = do
  mods <- gets $ M.elems . ircModulesByName
  forM_ mods $ \(Some modInfo) -> runModuleT f modInfo

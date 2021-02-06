{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | Support for the LB (LambdaBot) monad
module Lambdabot.State
  ( -- ** Functions to access the module's state
    MonadLBState(..)
  , readMS
  , writeMS
  , modifyMS

    -- ** Utility functions for modules that need state for each target.
  , GlobalPrivate -- (global)
  , mkGlobalPrivate
  , withPS
  , readPS
  , writePS
  , withGS
  , readGS
  , writeGS

    -- ** Handling global state
  , readGlobalState
  , writeGlobalState
  )
where

import qualified Lambdabot.File as Bot ( findLBFileForReading, findLBFileForWriting )
import qualified Lambdabot.Logging as Bot ( debugM, errorM, MonadLogging )
import qualified Lambdabot.Monad as Bot ( MonadLB(..) )
import qualified Lambdabot.Module as Bot ( LB, Module(moduleSerialize), ModuleInfo(moduleState, theModule, moduleName), ModuleT )
import qualified Lambdabot.Nick as Bot ( Nick )
import qualified Lambdabot.Command as Bot ( Cmd )
import qualified Lambdabot.Util as Bot ( io )
import qualified Lambdabot.Util.Serial as Bot ( Serial(serialize, deserialize) )

import Control.Concurrent.Lifted ( MVar, newMVar, readMVar, takeMVar, tryPutMVar )
import Control.Exception.Lifted as E ( SomeException(..), bracket, catch, evaluate )
import Control.Monad.Reader ( MonadIO(liftIO), MonadTrans(lift), asks )
import Control.Monad.Trans.Control ( MonadBaseControl, MonadTransControl(restoreT, liftWith) )
import qualified Data.ByteString.Char8 as P
import Data.IORef.Lifted ( newIORef, readIORef, writeIORef )
import qualified System.IO as I
import qualified System.FilePath as I
import qualified System.Directory as I

-- | Thread-safe modification of an MVar.
withMWriter :: MonadBaseControl IO m => MVar a -> (a -> (a -> m ()) -> m b) -> m b
withMWriter mvar f = bracket
  (do
    x   <- takeMVar mvar
    ref <- newIORef x
    return (x, ref)
  )
  (\(_, ref) -> tryPutMVar mvar =<< readIORef ref)
  (\(x, ref) -> f x $ writeIORef ref)

class Bot.MonadLB m => MonadLBState m where
    type LBState m

    -- | Update the module's private state.
    -- This is the preferred way of changing the state. The state will be locked
    -- until the body returns. The function is exception-safe, i.e. even if
    -- an error occurs or the thread is killed (e.g. because it deadlocked and
    -- therefore exceeded its time limit), the state from the last write operation
    -- will be restored. If the writer escapes, calling it will have no observable
    -- effect.
    -- @withMS@ is not composable, in the sense that a readMS from within the body
    -- will cause a dead-lock. However, all other possibilies to access the state
    -- that came to my mind had even more serious deficiencies such as being prone
    -- to race conditions or semantic obscurities.
    withMS :: (LBState m -> (LBState m -> m ()) -> m a) -> m a

instance Bot.MonadLB m => MonadLBState (Bot.ModuleT st m) where
  type LBState (Bot.ModuleT st m) = st
  withMS f = do
    moduleState <- asks Bot.moduleState
    withMWriter moduleState f

instance MonadLBState m => MonadLBState (Bot.Cmd m) where
  type LBState (Bot.Cmd m) = LBState m
  withMS f = do
    x <- liftWith $ \run -> withMS $ \st wr -> run (f st (lift . wr))
    restoreT (return x)

-- | Read the module's private state.
readMS :: MonadLBState m => m (LBState m)
readMS = withMS (\st _ -> return st)

-- | Modify the module's private state.
modifyMS :: MonadLBState m => (LBState m -> LBState m) -> m ()
modifyMS f = withMS $ \st wr -> wr (f st)

-- | Write the module's private state. Try to use withMS instead.
writeMS :: MonadLBState m => LBState m -> m ()
writeMS = modifyMS . const

-- | This datatype allows modules to conviently maintain both global
--   (i.e. for all clients they're interacting with) and private state.
--   It is implemented on top of readMS\/withMS.
--
-- This simple implementation is linear in the number of private states used.
data GlobalPrivate g p = GP {
  global :: !g,
  private :: ![(Bot.Nick, MVar (Maybe p))],
  maxSize :: Int
}

-- | Creates a @GlobalPrivate@ given the value of the global state. No private
--   state for clients will be created.
mkGlobalPrivate :: Int -> g -> GlobalPrivate g p
mkGlobalPrivate ms g = GP { global = g, private = [], maxSize = ms }

-- Needs a better interface. The with-functions are hardly useful.
-- | Writes private state. For now, it locks everything.
withPS
  :: (MonadLBState m, LBState m ~ GlobalPrivate g p)
  => Bot.Nick  -- ^ The target
  -> (Maybe p -> (Maybe p -> Bot.LB ()) -> Bot.LB a)
    -- ^ @Just x@ writes x in the user's private state, @Nothing@ removes it.
  -> m a
withPS who f = do
  mvar <- accessPS return id who
  Bot.lb $ withMWriter mvar f

-- | Reads private state.
readPS :: (MonadLBState m, LBState m ~ GlobalPrivate g p) => Bot.Nick -> m (Maybe p)
readPS = accessPS (liftIO . readMVar) (\_ -> return Nothing)

-- | Reads private state, executes one of the actions success and failure
-- which take an MVar and an action producing a @Nothing@ MVar, respectively.
accessPS
  :: (MonadLBState m, LBState m ~ GlobalPrivate g p)
  => (MVar (Maybe p) -> m a)
  -> (m (MVar (Maybe p)) -> m a)
  -> Bot.Nick
  -> m a
accessPS success failure who = withMS $ \state writer ->
  case lookup who $ private state of
    Just mvar -> do
      let newPrivate = (who, mvar) : filter ((/= who) . fst) (private state)
      length newPrivate `seq` writer (state { private = newPrivate })
      success mvar
    Nothing -> failure $ do
      mvar <- liftIO $ newMVar Nothing
      let newPrivate = take (maxSize state) $ (who, mvar) : private state
      length newPrivate `seq` writer (state { private = newPrivate })
      return mvar

-- | Writes global state. Locks everything
withGS
  :: (MonadLBState m, LBState m ~ GlobalPrivate g p)
  => (g -> (g -> m ()) -> m ())
  -> m ()
withGS f = withMS
  $ \state writer -> f (global state) $ \g -> writer $ state { global = g }

-- | Reads global state.
readGS :: (MonadLBState m, LBState m ~ GlobalPrivate g p) => m g
readGS = fmap global readMS


-- The old interface, as we don't wanna be too fancy right now.
writePS
  :: (MonadLBState m, LBState m ~ GlobalPrivate g p) => Bot.Nick -> Maybe p -> m ()
writePS who x = withPS who (\_ writer -> writer x)

writeGS :: (MonadLBState m, LBState m ~ GlobalPrivate g p) => g -> m ()
writeGS g = withGS (\_ writer -> writer g)

-- ---------------------------------------------------------------------
--
-- Handling global state
--

-- | Write the global state to a file
writeGlobalState :: Bot.ModuleT st Bot.LB ()
writeGlobalState = do
  thisModule <- asks Bot.theModule
  moduleName <- asks Bot.moduleName
  Bot.debugM ("saving state for module " ++ show moduleName)
  let nothingToWrite = pure ()
  let writer = writeGlobalStateToFile moduleName
  let write = serializeGlobalStateToFile writer nothingToWrite
  maybe nothingToWrite write $ Bot.moduleSerialize thisModule

serializeGlobalStateToFile :: MonadLBState m => (P.ByteString -> m ()) -> m () -> Bot.Serial (LBState m) -> m ()
serializeGlobalStateToFile write nothingToWrite ser = do
  moduleState <- readMS
  maybe nothingToWrite write $ Bot.serialize ser moduleState

writeGlobalStateToFile :: Bot.MonadLB m => FilePath -> P.ByteString -> m ()
writeGlobalStateToFile moduleName moduleState = do
  stateFile <- Bot.lb $ Bot.findLBFileForWriting moduleName
  let filePath = I.takeFileName stateFile
  let folderPath = I.takeDirectory stateFile
  (temporaryStateFile, handle) <- Bot.io $ I.openBinaryTempFileWithDefaultPermissions folderPath filePath
  Bot.io $ P.hPutStr handle moduleState >> I.hClose handle
  Bot.io $ I.renameFile temporaryStateFile stateFile

-- | Read the global state from a file
readGlobalState :: Bot.Module st -> String -> Bot.LB (Maybe st)
readGlobalState thisModule moduleName = do
  Bot.debugM $ "loading state for module " ++ show moduleName
  maybe (pure Nothing) (deserializeGlobalState moduleName) (Bot.moduleSerialize thisModule)

deserializeGlobalState :: FilePath -> Bot.Serial st -> Bot.LB (Maybe st)
deserializeGlobalState filePath engine = do
  maybeStateFilePath <- Bot.findLBFileForReading filePath
  maybeFileContents <- readGlobalStateFile maybeStateFilePath
  let anyException = handleDeserializationException maybeStateFilePath filePath
  let tryDeserialize = evaluate $ (Just $!) =<< (Bot.deserialize engine =<< maybeFileContents)
  tryDeserialize `E.catch` anyException

readGlobalStateFile :: MonadIO m => Maybe String -> m (Maybe P.ByteString)
readGlobalStateFile Nothing = pure Nothing
readGlobalStateFile (Just stateFilePath) = Bot.io $ do
  let anyException = \SomeException{} -> pure Nothing
  let tryReadFile = Just `fmap` P.readFile stateFilePath
  tryReadFile `E.catch` anyException

handleDeserializationException :: (Bot.MonadLogging m) => Maybe FilePath -> String -> SomeException -> m (Maybe st)
handleDeserializationException Nothing _ _ = pure Nothing
handleDeserializationException (Just stateFilePath) name e = do
  Bot.errorM $ "Error parsing state file for: " ++ name ++ ": " ++ show (e :: SomeException)
  Bot.errorM $ "Try removing: " ++ show stateFilePath
  pure Nothing
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Lambdabot.Module (
  Module (..),
  newModule,
  ModuleID,
  newModuleID,
  ModuleInfo (..),
  ModuleT,
  runModuleT,
  LB (..),
  CommandRef (..),
  OutputFilterRef (..),
  CallbackRef (..),
  ServerRef (..),
  IRCRState (..),
  IRCRWState (..),
  Callback,
  Server,
  OutputFilter,
) where

import Control.Concurrent (MVar)
import Control.Monad.Base (MonadBase (..))
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Exception (MonadException)
import Control.Monad.Identity (Identity)
import Control.Monad.Reader (
  MonadReader (..),
  ReaderT (..),
  asks,
 )
import Control.Monad.Trans (
  MonadIO (..),
  MonadTrans (..),
 )
import Control.Monad.Trans.Control (
  ComposeSt,
  MonadBaseControl (..),
  MonadTransControl (..),
  defaultLiftBaseWith,
  defaultRestoreM,
 )
import qualified Data.Dependent.Map as D
import Data.Dependent.Sum (DSum ())
import Data.IORef (IORef)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Some (Some)
import Data.Unique.Tag (GCompare, GEq, RealWorld, Tag, newTag)
import Lambdabot.ChanName (ChanName)
import Lambdabot.Command (Command)
import qualified Lambdabot.Command as Cmd
import Lambdabot.Config (Config, MonadConfig)
import Lambdabot.IRC (IrcMessage)
import Lambdabot.Logging (MonadLogging (..))
import Lambdabot.Nick (Nick)
import Lambdabot.Util.Serial (Serial)

------------------------------------------------------------------------

-- | The Module type class.
data Module st = Module
  { -- | If the module wants its state to be saved, this function should
    --   return a Serial.
    --
    --   The default implementation returns Nothing.
    moduleSerialize :: !(Maybe (Serial st))
  , -- | If the module maintains state, this method specifies the default state
    --   (for example in case the state can't be read from a state).
    --
    --   The default implementation returns an error and assumes the state is
    --   never accessed.
    moduleDefState :: !(LB st)
  , -- | Is the module sticky? Sticky modules (as well as static ones) can't be
    --   unloaded. By default, modules are not sticky.
    moduleSticky :: !Bool
  , -- | The commands the module listens to.
    moduleCmds :: !(ModuleT st LB [Cmd.Command (ModuleT st LB)])
  , -- | Initialize the module. The default implementation does nothing.
    moduleInit :: !(ModuleT st LB ())
  , -- | Finalize the module. The default implementation does nothing.
    moduleExit :: !(ModuleT st LB ())
  , -- | Process contextual input. A plugin that implements 'contextual'
    -- is able to respond to text not part of a normal command.
    contextual ::
      !( String -> --  the text
         Cmd.Cmd (ModuleT st LB) ()
       )
  }

------------------------------------------------------------------------

newModule :: Module st
newModule =
  Module
    { contextual = \_ -> return ()
    , moduleCmds = return []
    , moduleExit = return ()
    , moduleInit = return ()
    , moduleSticky = False
    , moduleSerialize = Nothing
    , moduleDefState = return $ error "state not initialized"
    }

newtype ModuleID st = ModuleID (Tag RealWorld st)
  deriving (GEq, GCompare)

newModuleID :: IO (ModuleID st)
newModuleID = ModuleID <$> newTag

-- | Info about a running module.
data ModuleInfo st = ModuleInfo
  { moduleName :: !String
  , moduleID :: !(ModuleID st)
  , theModule :: !(Module st)
  , moduleState :: !(MVar st)
  }

{- | This transformer encodes the additional information a module might
   need to access its name or its state.
-}
newtype ModuleT st m a = ModuleT {unModuleT :: ReaderT (ModuleInfo st) m a}
  deriving
    ( Applicative
    , Functor
    , Monad
    , MonadFail
    , MonadThrow
    , MonadCatch
    , MonadMask
    , MonadReader (ModuleInfo st)
    , MonadTrans
    , MonadIO
    , MonadException
    , MonadConfig
    )

runModuleT :: ModuleT st m a -> ModuleInfo st -> m a
runModuleT = runReaderT . unModuleT

instance MonadLogging m => MonadLogging (ModuleT st m) where
  getCurrentLogger = do
    parent <- lift getCurrentLogger
    self <- asks moduleName
    return (parent ++ ["Plugin", self])
  logM a b c = lift (logM a b c)

instance MonadBase b m => MonadBase b (ModuleT st m) where
  liftBase = lift . liftBase

instance MonadTransControl (ModuleT st) where
  type StT (ModuleT st) a = a
  liftWith f = do
    r <- ModuleT ask
    lift $ f $ \t -> runModuleT t r
  restoreT = lift
  {-# INLINE liftWith #-}
  {-# INLINE restoreT #-}

instance MonadBaseControl b m => MonadBaseControl b (ModuleT st m) where
  type StM (ModuleT st m) a = ComposeSt (ModuleT st) m a
  liftBaseWith = defaultLiftBaseWith
  restoreM = defaultRestoreM
  {-# INLINE liftBaseWith #-}
  {-# INLINE restoreM #-}

type Callback st = IrcMessage -> ModuleT st LB ()

type OutputFilter st = Nick -> [String] -> ModuleT st LB [String]

type Server st = IrcMessage -> ModuleT st LB ()

newtype CallbackRef st = CallbackRef (Callback st)

newtype CommandRef st = CommandRef (Command (ModuleT st LB))

newtype OutputFilterRef st = OutputFilterRef (OutputFilter st)

newtype ServerRef st = ServerRef (Server st)

------------------------------------------------------------------------
--
-- Lambdabot state
--

-- | Global read-only state.
data IRCRState = IRCRState
  { ircInitDoneMVar :: MVar ()
  , ircQuitMVar :: MVar ()
  , ircConfig :: D.DMap Config Identity
  }

-- | Global read\/write state.
data IRCRWState = IRCRWState
  { ircServerMap :: M.Map String (DSum ModuleID ServerRef)
  , ircPrivilegedUsers :: S.Set Nick
  , ircIgnoredUsers :: S.Set Nick
  , -- | maps channel names to topics
    ircChannels :: M.Map ChanName String
  , -- | lists servers to which to reconnect on failure (one-time or always)
    ircPersists :: M.Map String Bool
  , ircModulesByName :: M.Map String (Some ModuleInfo)
  , ircModulesByID :: D.DMap ModuleID ModuleInfo
  , ircCallbacks :: M.Map String (D.DMap ModuleID CallbackRef)
  , -- | Output filters, invoked from right to left
    ircOutputFilters :: [DSum ModuleID OutputFilterRef]
  , ircCommands :: M.Map String (DSum ModuleID CommandRef)
  }

-- ---------------------------------------------------------------------
--
-- The LB (LambdaBot) monad
--

{- | The IRC Monad. The reader transformer holds information about the
   connection to the IRC server.

 instances Monad, Functor, MonadIO, MonadState, MonadError
-}
newtype LB a = LB {unLB :: ReaderT (IRCRState, IORef IRCRWState) IO a}
  deriving (Functor, Applicative, Monad, MonadFail, MonadThrow, MonadCatch, MonadMask, MonadIO, MonadException)

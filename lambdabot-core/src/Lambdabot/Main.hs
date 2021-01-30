module Lambdabot.Main (
  lambdabotVersion,
  Config,
  DSum (..),
  (==>),
  lambdabotMain,
  Modules,
  module Lambdabot.Plugin.Core,
  Priority (..),
) where

import Lambdabot.Bot (ircLoadModule, ircUnloadModule)
import Lambdabot.Config (Config, MonadConfig (getConfig))
import Lambdabot.Logging (Priority (..), errorM, infoM)
import Lambdabot.Module (LB, Module)
import Lambdabot.Monad (
  initRoState,
  initRwState,
  listModules,
  reportInitDone,
  runLB,
  waitForQuit,
 )
import Lambdabot.Plugin.Core (
  basePlugin,
  commandPrefixes,
  composePlugin,
  consoleLogFormat,
  consoleLogHandle,
  consoleLogLevel,
  corePlugins,
  dataDir,
  disabledCommands,
  editDistanceLimit,
  enableInsults,
  helpPlugin,
  lbRootLoggerPath,
  lbVersion,
  morePlugin,
  offlineRCPlugin,
  onShutdownCmds,
  onStartupCmds,
  outputDir,
  replaceRootLogger,
  systemPlugin,
  textWidth,
  uncaughtExceptionHandler,
  versionPlugin,
 )
import Lambdabot.Util (io)
import Lambdabot.Util.Signals (withIrcSignalCatch)

import Control.Exception.Lifted as E (Exception (fromException), SomeException, bracket_, catch)
import Control.Monad.Identity (Identity)
import Data.Dependent.Sum (DSum ((:=>)), (==>))
import Data.IORef (newIORef)
import Data.Some (Some (..))
import Data.Version (Version)
import Network.Socket (withSocketsDo)
import Paths_lambdabot_core (version)
import System.Exit (ExitCode (..))
import System.Log.Formatter (simpleLogFormatter)
import System.Log.Handler.Simple (GenericHandler (formatter), streamHandler)
import qualified System.Log.Logger as L

lambdabotVersion :: Version
lambdabotVersion = version

setupLogging :: LB ()
setupLogging = do
  stream <- getConfig consoleLogHandle
  level <- getConfig consoleLogLevel
  format <- getConfig consoleLogFormat

  unformattedHandler <- io (streamHandler stream level)
  let consoleHandler =
        unformattedHandler{formatter = simpleLogFormatter format}

  setRoot <- getConfig replaceRootLogger

  io $
    if setRoot
      then
        L.updateGlobalLogger
          L.rootLoggerName
          (L.setLevel level . L.setHandlers [consoleHandler])
      else
        L.updateGlobalLogger
          "Lambdabot"
          (L.setLevel level . L.addHandler consoleHandler)

{- | The Lambdabot entry point.
 Initialise plugins, connect, and run the bot in the LB monad

 Also, handle any fatal exceptions (such as non-recoverable signals),
 (i.e. print a message and exit). Non-fatal exceptions should be dealt
 with in the mainLoop or further down.
-}
lambdabotMain :: Modules -> [DSum Config Identity] -> IO ExitCode
lambdabotMain initialise cfg = withSocketsDo . withIrcSignalCatch $ do
  rost <- initRoState cfg
  rwst <- newIORef initRwState
  let runMainLoop = runLB (lambdabotRun initialise) (rost, rwst)
  E.catch runMainLoop showExceptionAndExit

showExceptionAndExit :: SomeException -> IO ExitCode
showExceptionAndExit exception = case fromException exception of
  Just code -> return code
  Nothing -> do
    errorM (show exception)
    return (ExitFailure 1)

showExceptionAndContinue :: SomeException -> LB ()
showExceptionAndContinue exception = errorM $ show exception

lambdabotRun :: Modules -> LB ExitCode
lambdabotRun ms = do
  setupLogging
  infoM "Initialising plugins"
  withModules ms $ do
    infoM "Done loading plugins"
    reportInitDone
    E.catch waitForQuit showExceptionAndContinue

  -- clean up any dynamically loaded modules
  mapM_ ircUnloadModule =<< listModules
  io L.removeAllHandlers
  io $ putStrLn ""
  return ExitSuccess

------------------------------------------------------------------------

type Modules = [(String, Some Module)]

withModules :: Modules -> LB a -> LB a
withModules [] = id
withModules ((n, Some m) : ms) = withModule n m . withModules ms

withModule :: String -> Module st -> LB a -> LB a
withModule name m = bracket_ (ircLoadModule name m) (ircUnloadModule name)

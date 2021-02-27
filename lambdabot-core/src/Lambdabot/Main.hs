{-# LANGUAGE TemplateHaskell #-}

module Lambdabot.Main (
  consoleLogLevel,
  enableInsults,
  lbVersion,
  onStartupCmds,
  Config,
  lambdabotMain,
  lambdabotVersion,
  Modules,
  modules,
  Priority (..),
) where

import Lambdabot.Bot (ircLoadModule, ircUnloadModule)
import Lambdabot.Config (Config, MonadConfig (getConfig))
import Lambdabot.Config.Core (consoleLogFormat, consoleLogHandle, consoleLogLevel, enableInsults, lbVersion, onStartupCmds, replaceRootLogger)
import Lambdabot.Logging (GenericHandler (formatter), Priority (..), addHandler, errorM, infoM, removeAllHandlers, rootLoggerName, setHandlers, setLevel, simpleLogFormatter, streamHandler, updateGlobalLogger)
import Lambdabot.Module (LB, Module)
import Lambdabot.Monad (initRoState, initRwState, listModules, reportInitDone, runLB, waitForQuit)
import Lambdabot.Util (io)
import Lambdabot.Util.Signals (withIrcSignalCatch)

import Control.Exception.Lifted as E (Exception (fromException), SomeException, bracket_, catch)
import Control.Monad.Identity (Identity)
import Data.Dependent.Sum (DSum)
import Data.IORef (newIORef)
import Data.List (nub)
import Data.Some (Some (..))
import Data.Version (Version)
import Language.Haskell.TH (Exp, Q, listE)
import Language.Haskell.TH.Lib (varE)
import Language.Haskell.TH.Syntax (mkName)
import Network.Socket (withSocketsDo)
import Paths_lambdabot_core (version)
import System.Exit (ExitCode (..))

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
        updateGlobalLogger
          rootLoggerName
          (setLevel level . setHandlers [consoleHandler])
      else
        updateGlobalLogger
          "Lambdabot"
          (setLevel level . addHandler consoleHandler)

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
  io removeAllHandlers
  io $ putStrLn ""
  return ExitSuccess

------------------------------------------------------------------------

type Modules = [(String, Some Module)]

modules :: [String] -> Q Exp
modules xs = [|$(listE $ map instalify (nub xs))|]
 where
  instalify x =
    let module' = varE $ mkName (x ++ "Plugin")
     in [|(x, Some $module')|]

withModules :: Modules -> LB a -> LB a
withModules [] = id
withModules ((n, Some m) : ms) = withModule n m . withModules ms

withModule :: String -> Module st -> LB a -> LB a
withModule name m = bracket_ (ircLoadModule name m) (ircUnloadModule name)

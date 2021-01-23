{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Lambdabot.Config.Core (
  commandPrefixes,
  disabledCommands,
  editDistanceLimit,
  enableInsults,
  onStartupCmds,
  onShutdownCmds,
  outputDir,
  dataDir,
  lbVersion,
  textWidth,
  uncaughtExceptionHandler,
  replaceRootLogger,
  lbRootLoggerPath,
  consoleLogHandle,
  consoleLogLevel,
  consoleLogFormat,
) where

import Lambdabot.Config (config, configWithMerge)
import Lambdabot.Logging (Priority (WARNING), errorM)

import Control.Exception (SomeException)
import Data.Version (Version (Version))
import System.IO (Handle, stderr)

-------------------------------------
-- Core configuration variables

config "commandPrefixes" [t|[String]|] [|["?"]|]
config "disabledCommands" [t|[String]|] [|[]|]
config "editDistanceLimit" [t|Int|] [|3 :: Int|]
config "enableInsults" [t|Bool|] [|True|]
configWithMerge [|(++)|] "onStartupCmds" [t|[String]|] [|["rc scripts/onstartup.rc"]|]
configWithMerge [|(++)|] "onShutdownCmds" [t|[String]|] [|["rc scripts/onshutdown.rc"]|]
config "outputDir" [t|FilePath|] [|"State/"|]

-- the dataDir variable will be filled by lambdabot's executable
config "dataDir" [t|FilePath|] [|"."|]

-- ditto for lbVersion
config "lbVersion" [t|Version|] [|Version [] []|]

-- IRC maximum msg length, minus a bit for safety.
config "textWidth" [t|Int|] [|200 :: Int|]

-- basic logging.  for more complex setups, configure directly using System.Log.Logger
config "replaceRootLogger" [t|Bool|] [|True|]
config "lbRootLoggerPath" [t|[String]|] [|[]|]
config "consoleLogHandle" [t|Handle|] [|stderr|]
config "consoleLogLevel" [t|Priority|] [|WARNING|]
config "consoleLogFormat" [t|String|] [|"[$prio] $loggername: $msg"|]

--------------------------------------------
-- Default values with longer definitions

defaultIrcHandler :: SomeException -> IO ()
defaultIrcHandler = errorM . ("Main: caught (and ignoring) " ++) . show

type DIH = SomeException -> IO ()

-- work around a TemplateHaskell bug in ghc-8.6.1
-- see https://ghc.haskell.org/trac/ghc/ticket/15815

config "uncaughtExceptionHandler" [t|DIH|] [|defaultIrcHandler|]

--
-- Copyright (c) 2006 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)
--
-- Syntactic sugar for developing plugins.
-- Simplifies import lists, and abstracts over common patterns
--
module Lambdabot.Plugin (
  Module (..),
  ModuleT,
  newModule,
  LB,
  MonadLB (..),
  ChanName,
  mkCN,
  getCN,
  Nick (..),
  ircPrivmsg,
  module Lambdabot.Config,
  module Lambdabot.Config.Core,
  module Lambdabot.Command,
  module Lambdabot.State,
  module Lambdabot.File,
  module Lambdabot.Util.Serial,
) where

import Lambdabot.Bot (ircPrivmsg)
import Lambdabot.ChanName (ChanName, getCN, mkCN)
import Lambdabot.Command (
  Cmd,
  Command (..),
  cmdNames,
  command,
  getCmdName,
  getLambdabotName,
  getSender,
  getServer,
  getTarget,
  readNick,
  say,
  showNick,
  withMsg,
 )
import Lambdabot.Config (Config, MonadConfig (..), config, configWithMerge, getConfigDefault, mergeConfig)
import Lambdabot.Config.Core (
  commandPrefixes,
  consoleLogFormat,
  consoleLogHandle,
  consoleLogLevel,
  dataDir,
  disabledCommands,
  editDistanceLimit,
  enableInsults,
  lbRootLoggerPath,
  lbVersion,
  onShutdownCmds,
  onStartupCmds,
  outputDir,
  replaceRootLogger,
  textWidth,
  uncaughtExceptionHandler,
 )
import Lambdabot.File (
  findLBFile,
  findLBFileForReading,
  findLBFileForWriting,
  findOrCreateLBFile,
  outputDir,
  stateDir,
 )
import Lambdabot.Module (LB, Module (..), ModuleT, newModule)
import Lambdabot.Monad (MonadLB (..))
import Lambdabot.Nick (Nick (..))
import Lambdabot.State (
  GlobalPrivate,
  MonadLBState (..),
  mkGlobalPrivate,
  modifyMS,
  readGS,
  readGlobalState,
  readMS,
  readPS,
  withGS,
  withPS,
  writeGS,
  writeGlobalState,
  writeMS,
  writePS,
 )
import Lambdabot.Util (readPackedEntry)
import Lambdabot.Util.Serial (
  Packable (..),
  Serial (..),
  assocListPackedSerial,
  mapListPackedSerial,
  mapPackedSerial,
  mapSerial,
  readM,
  readOnly,
  readPackedEntry,
  stdSerial,
 )

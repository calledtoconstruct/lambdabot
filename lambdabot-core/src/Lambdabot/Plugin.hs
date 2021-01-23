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
  lim80,
  ios80,
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
import Lambdabot.Util (io, limitStr, readPackedEntry)
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

import Codec.Binary.UTF8.String (decodeString, encodeString)
import Control.Monad ((<=<))
import Control.Monad.Trans (MonadIO, MonadTrans (lift))
import Data.Char (isControl, isSpace)

lim80 :: Monad m => m String -> Cmd m ()
lim80 action = do
  to <- getTarget
  let lim = case nName to of
        ('#' : _) -> take 3 . map (limitStr 80) -- message to channel: be nice
        _ -> id -- private message: get everything
      spaceOut = unlines . lim . map (' ' :) . lines
      removeControl = filter (\x -> isSpace x || not (isControl x))
  say <=< lift $
    fmap
      (encodeString . spaceOut . removeControl . decodeString)
      action

{- | convenience, similar to ios but also cut output to channel to 80 characters
 usage:  @process _ _ to _ s = ios80 to (plugs s)@
-}
ios80 :: MonadIO m => IO String -> Cmd m ()
ios80 = lim80 . io

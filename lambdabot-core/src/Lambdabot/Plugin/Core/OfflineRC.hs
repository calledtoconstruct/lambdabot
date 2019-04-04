
{-# OPTIONS_GHC -Wmissing-signatures #-}

module Lambdabot.Plugin.Core.OfflineRC
  ( offlineRCPlugin
  )
where

import           Lambdabot.Config.Core
import           Lambdabot.IRC
import           Lambdabot.Monad
import           Lambdabot.Plugin
import           Lambdabot.Util

import           Control.Concurrent.Lifted
import           Control.Exception.Lifted       ( evaluate
                                                , finally
                                                )
import           Control.Monad                  ( void
                                                , when
                                                , unless
                                                )
import           Control.Monad.State            ( gets
                                                , modify
                                                )
import           Control.Monad.Trans            ( lift
                                                , liftIO
                                                )
import           Data.Char
import qualified Data.Map                      as M
import qualified Data.Set                      as S
import           System.Console.Haskeline       ( InputT
                                                , Settings(..)
                                                , runInputT
                                                , defaultSettings
                                                , getInputLine
                                                )
import           System.IO
import           System.Timeout.Lifted
import           Codec.Binary.UTF8.String

-- We need to track the number of active sourcings so that we can
-- unregister the server (-> allow the bot to quit) when it is not
-- being used.
type OfflineRCState = Integer

type OfflineRC = ModuleT OfflineRCState LB

offlineRCPlugin :: Module OfflineRCState
offlineRCPlugin = newModule
  { moduleDefState = return 0
  , moduleInit     = initializeModule
  , moduleExit     = finalizeModule
  , moduleCmds     =
    return
      [ (command "offline") { privileged = True
                            , help       = say "offline. Start a repl"
                            , process    = startOfflineConsole
                            }
      , (command "shutdown") { privileged = True
                             , help = say "shutdown, have the bot shutdown"
                             , process = stopOfflineConsole
                             }
      , (command "rc")
        { privileged = True
        , help       =
          say
            "rc name. Read a file of commands (asynchronously). TODO: better name."
        , process    = runScript
        }
      ]
  }

initializeModule :: ModuleT OfflineRCState LB ()
initializeModule = do
  lb . modify $ loadPrivilegedUsers
  void . forkUnmasked $ do
    waitForInit
    lockRC
    cmds <- getConfig onStartupCmds
    mapM_ feed cmds `finally` unlockRC

loadPrivilegedUsers :: IRCRWState -> IRCRWState
loadPrivilegedUsers moduleState = moduleState
  { ircPrivilegedUsers = S.insert (Nick "offlinerc" "null")
                                  (ircPrivilegedUsers moduleState)
  }

finalizeModule :: ModuleT OfflineRCState LB ()
finalizeModule = void . forkUnmasked $ do
  lockRC
  cmds <- getConfig onShutdownCmds
  mapM_ feed cmds `finally` unlockRC

startOfflineConsole :: b -> Cmd (ModuleT OfflineRCState LB) ()
startOfflineConsole = const . lift $ do
  lockRC
  histFile <- lb $ findLBFileForWriting "offlinerc"
  let settings = defaultSettings { historyFile = Just histFile }
  _ <- fork (runInputT settings replLoop `finally` unlockRC)
  return ()

stopOfflineConsole :: p -> Cmd OfflineRC ()
stopOfflineConsole _ = do
  runScript "scripts/shutdown.rc"
  lift $ unregisterServer "offlinerc"
  lift stopRC

runScript :: String -> Cmd OfflineRC ()
runScript fileName = lift $ do
  txt <- io $ readFile fileName
  io $ evaluate $ foldr seq () txt
  let linesOfText = lines txt
  lockRC
  _ <- fork $ finally (mapM_ feed linesOfText) unlockRC
  return ()

feed :: String -> OfflineRC ()
feed msg = do
  cmdPrefix <- fmap head (getConfig commandPrefixes)
  let msg' = case msg of
        '>' : xs -> cmdPrefix ++ "run " ++ xs
        '!' : xs -> xs
        _        -> cmdPrefix ++ dropWhile (== ' ') msg
  -- note that `msg'` is unicode, but lambdabot wants utf-8 lists of bytes
  lb . void . timeout (15 * 1000 * 1000) . received $ IrcMessage
    { ircMsgServer  = "offlinerc"
    , ircMsgLBName  = "offline"
    , ircMsgPrefix  = "null!n=user@null"
    , ircMsgCommand = "PRIVMSG"
    , ircMsgParams  = ["offline", ":" ++ encodeString msg']
    }

handleMsg :: IrcMessage -> OfflineRC ()
handleMsg msg = liftIO $ do
  let str = case (tail . ircMsgParams) msg of
        []      -> []
        (x : _) -> tail x
    -- str contains utf-8 list of bytes; convert to unicode
  putStrLn (decodeString str)
  hFlush stdout

replLoop :: InputT OfflineRC ()
replLoop = do
  line <- getInputLine "lambdabot> "
  case line of
    Nothing -> return ()
    Just x  -> do
      let s' = dropWhile isSpace x
      unless (null s') $ lift $ feed s'
      continue <- lift $ lift $ gets (M.member "offlinerc" . ircPersists)
      when continue replLoop

stopRC :: OfflineRC ()
stopRC = withMS $ \cur writ -> do
  lift $ modify $ \state' ->
    state' { ircPersists = M.insert "offlinerc" True $ ircPersists state' }
  writ (cur + 1)

lockRC :: OfflineRC ()
lockRC = withMS $ \cur writ -> do
  when (cur == 0) $ do
    registerServer "offlinerc" handleMsg
    lift $ modify $ \state' ->
      state' { ircPersists = M.insert "offlinerc" True $ ircPersists state' }
  writ (cur + 1)

unlockRC :: OfflineRC ()
unlockRC = withMS $ \cur writ -> do
  when (cur == 1) $ unregisterServer "offlinerc"
  writ (cur - 1)

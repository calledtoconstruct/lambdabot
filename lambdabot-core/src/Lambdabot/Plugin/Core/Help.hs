-- | Provide help for plugins
module Lambdabot.Plugin.Core.Help (
  helpPlugin,
) where

import Lambdabot.Command (
  Cmd,
  Command (help, process),
  cmdNames,
  command,
  execCmd,
  getTarget,
  say,
  withMsg,
 )
import Lambdabot.Message (Message)
import Lambdabot.Module (
  LB,
  Module (moduleCmds),
  ModuleInfo (theModule),
  newModule,
 )
import Lambdabot.Monad (MonadLB (..), inModuleNamed, withCommand)
import Lambdabot.Plugin (Nick)
import Lambdabot.Util (showClean)

import Control.Monad.Reader (asks)

helpPlugin :: Module ()
helpPlugin =
  newModule
    { moduleCmds =
        return
          [ (command "help")
              { help =
                  say
                    "help <command>. Ask for help for <command>. Try 'list' for all commands"
              , process = executeHelp
              }
          ]
    }

executeHelp :: MonadLB m => String -> Cmd m ()
executeHelp args = withMsg $ \msg -> do
  tgt <- getTarget
  lb (doHelp msg tgt args) >>= mapM_ say

moduleHelp ::
  (Monad m, Message a) => Command m -> a -> Nick -> String -> m [String]
moduleHelp theCmd = execCmd $ help theCmd

-- If a target is a command, find the associated help, otherwise if it's
-- a module, return a list of commands that module implements.
doHelp :: Message t => t -> Nick -> String -> LB [String]
doHelp msg tgt [] = doHelp msg tgt "help"
doHelp msg tgt rest =
  withCommand
    arg
    ( inModuleNamed
        arg
        (doHelp msg tgt "help")
        ( do
            cmds <- moduleCmds =<< asks theModule
            let ss = cmds >>= cmdNames
            let s
                  | null ss = arg ++ " is a module."
                  | otherwise = arg ++ " provides: " ++ showClean ss
            return [s]
        )
    )
    (\theCmd -> moduleHelp theCmd msg tgt arg)
 where
  (arg : _) = words rest

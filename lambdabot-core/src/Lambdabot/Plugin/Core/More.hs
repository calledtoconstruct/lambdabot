-- | Support for more(1) buffering
module Lambdabot.Plugin.Core.More (
  morePlugin,
) where

import Lambdabot.Bot (ircPrivmsg')
import Lambdabot.Monad (MonadLB (lb), registerOutputFilter)
import Lambdabot.Plugin (
  Command (help, process),
  GlobalPrivate,
  LB,
  Module (moduleCmds, moduleDefState, moduleInit),
  ModuleT,
  Nick,
  command,
  getTarget,
  mkGlobalPrivate,
  newModule,
  readPS,
  say,
  writePS,
 )

import Control.Monad.Trans (MonadTrans (lift))

type MoreState = GlobalPrivate () [String]

type More = ModuleT MoreState LB

-- the @more state is handled centrally
morePlugin :: Module (GlobalPrivate () [String])
morePlugin =
  newModule
    { moduleDefState = return $ mkGlobalPrivate 20 ()
    , moduleInit = registerOutputFilter moreFilter
    , moduleCmds =
        return
          [ (command "more")
              { help = say "?more. Return more output from the bot buffer."
              , process =
                  \_ -> do
                    target <- getTarget
                    morestate <- readPS target
                    case morestate of
                      Nothing -> return ()
                      Just ls ->
                        lift (moreFilter target ls)
                          >>= mapM_ (lb . ircPrivmsg' target)
              }
          ]
    }

moreFilter :: Nick -> [String] -> More [String]
moreFilter target msglines = do
  let (morelines, thislines) = case drop (maxLines + 2) msglines of
        [] -> ([], msglines)
        _ -> (drop maxLines msglines, take maxLines msglines)
  writePS target $ if null morelines then Nothing else Just morelines
  return $
    thislines
      ++ ['[' : shows (length morelines) " ?more lines]" | not (null morelines)]
 where
  maxLines = 5 -- arbitrary, really

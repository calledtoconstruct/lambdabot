-- | Support for more(1) buffering
module Lambdabot.Plugin.Core.More
  ( morePlugin
  )
where

import           Lambdabot.Bot
import           Lambdabot.Monad
import           Lambdabot.Plugin

import           Control.Monad.Trans

type MoreState = GlobalPrivate () [String]

type More = ModuleT MoreState LB

-- the @more state is handled centrally
morePlugin :: Module (GlobalPrivate () [String])
morePlugin = newModule
  { moduleDefState = return $ mkGlobalPrivate 20 ()
  , moduleInit     = registerOutputFilter moreFilter
  , moduleCmds     =
    return
      [ (command "more")
          { help    = say "?more. Return more output from the bot buffer."
          , process =
            \_ -> do
              target    <- getTarget
              morestate <- readPS target
              case morestate of
                Nothing -> return ()
                Just ls -> lift (moreFilter target ls)
                  >>= mapM_ (lb . ircPrivmsg' target)
          }
      ]
  }

moreFilter :: Nick -> [String] -> More [String]
moreFilter target msglines = do
  let (morelines, thislines) = case drop (maxLines + 2) msglines of
        [] -> ([], msglines)
        _  -> (drop maxLines msglines, take maxLines msglines)
  writePS target $ if null morelines then Nothing else Just morelines
  return $ thislines ++ if null morelines
    then []
    else ['[' : shows (length morelines) " ?more lines]"]
  where maxLines = 5 -- arbitrary, really

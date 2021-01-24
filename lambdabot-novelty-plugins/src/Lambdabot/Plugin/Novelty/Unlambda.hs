-- Copyright (c) 2006 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)
--

{- | A plugin for the Haskell interpreter for the unlambda language

 http://www.madore.org/~david/programs/unlambda/
-}
module Lambdabot.Plugin.Novelty.Unlambda (
  unlambdaPlugin,
) where

import Control.Monad.IO.Class (MonadIO)
import Lambdabot.Config.Novelty (unlambdaBinary)
import Lambdabot.Plugin (
  Cmd,
  Module (),
  MonadConfig,
  command,
  getConfig,
  help,
  moduleCmds,
  newModule,
  process,
  say,
 )
import Lambdabot.Util.Process (run)
import Text.Regex.TDFA ((=~))
import Lambdabot.Util (io)
import Lambdabot.Command (lineify)

unlambdaPlugin :: Module ()
unlambdaPlugin =
  newModule
    { moduleCmds =
        return
          [ (command "unlambda")
              { help = say "unlambda <expr>. Evaluate an unlambda expression"
              , process = unlambda
              }
          ]
    }

unlambda :: (MonadConfig m, MonadIO m) => String -> Cmd m ()
unlambda msg = do
  binary <- getConfig unlambdaBinary
  result <- io (run binary msg scrub)
  reply <- lineify [result]
  say reply

scrub :: String -> String
scrub = unlines . take 6 . map (' ' :) . lines . cleanit

cleanit :: String -> String
cleanit s
  | s =~ terminated = "Terminated\n"
  | otherwise = s
 where
  terminated = "waitForProc"

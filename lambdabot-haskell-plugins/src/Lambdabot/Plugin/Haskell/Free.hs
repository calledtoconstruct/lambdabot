{- | Free theorems plugin
 Andrew Bromage, 2006
-}
module Lambdabot.Plugin.Haskell.Free (freePlugin) where

import Lambdabot.Plugin (Command (help, process), Module (moduleCmds), command, newModule, say)
import Lambdabot.Plugin.Haskell.Free.FreeTheorem (freeTheoremStr)
import Lambdabot.Plugin.Haskell.Type (queryGHCI)

freePlugin :: Module ()
freePlugin =
  newModule
    { moduleCmds =
        return
          [ (command "free")
              { help = say "free <ident>. Generate theorems for free"
              , process = \xs -> do
                  result <- freeTheoremStr (queryGHCI ":t") xs
                  say . unwords . lines $ result
              }
          ]
    }

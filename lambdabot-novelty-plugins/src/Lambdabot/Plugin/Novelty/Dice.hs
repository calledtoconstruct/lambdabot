-- | This module is for throwing dice for e.g. RPGs. (\@dice 3d6+2)

-- Original version copyright Einar Karttunen <ekarttun@cs.helsinki.fi> 2005-04-06.
-- Massive rewrite circa 2008-10-20 copyright James Cook <mokus@deepbondi.net>

module Lambdabot.Plugin.Novelty.Dice
  ( dicePlugin
  )
where

import           Lambdabot.Plugin               ( Module
                                                , LB
                                                , Cmd
                                                , moduleCmds
                                                , aliases
                                                , help
                                                , command
                                                , ModuleT
                                                , process
                                                , contextual
                                                , newModule
                                                , showNick
                                                , getSender
                                                , say
                                                )
import           Lambdabot.Util                 ( io
                                                , limitStr
                                                )

import           Control.Monad                  ( when )

import           Data.List                      ( intercalate )
import           Data.Random.Dice               ( rollEm )

type Dice = ModuleT () LB

dicePlugin :: Module ()
dicePlugin = newModule
  { moduleCmds =
    return
      [ (command "dice")
          { aliases = ["roll"]
          , help    =
            say "?dice <expr>. Throw random dice. <expr> is of the form 3d6+2."
          , process = doDice True
          }
      ]
  , contextual = doDice False
  }

doDice :: Bool -> String -> Cmd Dice ()
doDice printErrs text = do
  user   <- showNick =<< getSender
  result <- io $ rollEm text
  case result of
    Left  err -> when printErrs $ say $ trimError err
    Right str -> say $ limitStr 75 $ user ++ ": " ++ str
  where trimError = intercalate ": " . tail . lines . show

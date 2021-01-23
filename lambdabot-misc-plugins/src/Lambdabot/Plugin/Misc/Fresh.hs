{- | Haskell project name generation
 semi-joke
-}
module Lambdabot.Plugin.Misc.Fresh (freshPlugin) where

import Control.Monad.Trans (lift)
import Data.Char (chr, ord)
import Lambdabot.Plugin (
  LB,
  Module,
  ModuleT,
  command,
  help,
  moduleCmds,
  moduleDefState,
  moduleSerialize,
  newModule,
  process,
  say,
  stdSerial,
  withMS,
 )

type Fresh = ModuleT Integer LB

freshPlugin :: Module Integer
freshPlugin =
  newModule
    { moduleDefState = return 0
    , moduleSerialize = Just stdSerial
    , moduleCmds =
        return
          [ (command "freshname")
              { help = say "freshname. Return a unique Haskell project name."
              , process = \_ -> lift fresh >>= say
              }
          ]
    }

fresh :: Fresh String
fresh = withMS $ \n f -> do
  f (n + 1)
  return ("Ha" ++ reverse (asName n))

asName :: Integer -> String
asName i
  | i == 0 = [chr (ord 'a')]
  | r == 0 = [chr (ord 'a' + fromIntegral a)]
  | otherwise = chr (ord 'a' + fromIntegral a) : asName r
 where
  (r, a) = i `quotRem` 26

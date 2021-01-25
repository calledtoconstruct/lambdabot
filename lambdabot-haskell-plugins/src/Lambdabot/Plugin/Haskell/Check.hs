-- Copyright (c) 6 DonStewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)

-- | Test a property with QuickCheck
module Lambdabot.Plugin.Haskell.Check (checkPlugin) where

import Lambdabot.Command (lineify)
import Lambdabot.Plugin (
  Command (help, process),
  Module (moduleCmds),
  MonadConfig,
  MonadLB,
  command,
  newModule,
  say,
 )
import Lambdabot.Plugin.Haskell.Eval (runGHC)

import Codec.Binary.UTF8.String ( decodeString )
import qualified Language.Haskell.Exts.Simple as Hs

checkPlugin :: Module ()
checkPlugin =
  newModule
    { moduleCmds =
        return
          [ (command "check")
              { help = do
                  say "check <expr>"
                  say "You have QuickCheck and 3 seconds. Prove something."
              , process = \rest -> do
                  result <- check rest
                  reply <- lineify [result]
                  say reply
              }
          ]
    }

check :: (MonadLB m, MonadConfig m) => String -> m String
check src = case Hs.parseExp (decodeString src) of
  Hs.ParseFailed l e -> return (Hs.prettyPrint l ++ ':' : e)
  Hs.ParseOk{} -> postProcess `fmap` runGHC ("text (myquickcheck (" ++ src ++ "))")

postProcess :: [Char] -> String
postProcess xs =
  let (first, rest) = splitAt 1 $ map (unwords . words) $ lines xs
   in unlines $ first ++ [unwords rest | not $ null rest]

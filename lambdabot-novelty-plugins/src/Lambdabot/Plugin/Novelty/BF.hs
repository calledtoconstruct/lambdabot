-- Copyright (c) 2006 Jason Dagit - http://www.codersbase.com/
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)

-- | A plugin for the Haskell interpreter for the brainf*ck language
-- http://www.muppetlabs.com/~breadbox/bf/
module Lambdabot.Plugin.Novelty.BF
  ( bfPlugin
  )
where

import           Lambdabot.Config.Novelty
import           Lambdabot.Plugin               ( Module
                                                , newModule
                                                , say
                                                , help
                                                , moduleCmds
                                                , process
                                                , command
                                                , ios80
                                                )
import           Lambdabot.Util.Process         ( run )

import           Data.Char                      ( ord )
import           Text.Regex.TDFA                ( (=~) )

bfPlugin :: Module ()
bfPlugin = newModule
  { moduleCmds = return
                   [ (command "bf")
                       { help = say "bf <expr>. Evaluate a brainf*ck expression"
                       , process = \msg -> do
                                     let bf = "bf" -- getConfig bfBinary
                                     ios80 (run bf msg scrub)
                       }
                   ]
  }

-- Clean up output
scrub :: String -> String
scrub =
  unlines . take 6 . map (' ' :) . filter (not . null) . map cleanit . lines

cleanit :: String -> String
cleanit s | s =~ terminated = "Terminated\n"
          | otherwise       = filter printable s
 where
  terminated = "waitForProc"
  -- the printable ascii chars are in the range [32 .. 126]
  -- according to wikipedia:
  -- http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters
  printable x = 31 < ord x && ord x < 127


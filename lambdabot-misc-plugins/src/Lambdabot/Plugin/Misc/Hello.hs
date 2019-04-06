--
-- | Hello world plugin
--
module Lambdabot.Plugin.Misc.Hello
  ( helloPlugin
  )
where

import           Lambdabot.Plugin               ( Module
                                                , moduleCmds
                                                , aliases
                                                , help
                                                , process
                                                , say
                                                , command
                                                , newModule
                                                )

helloPlugin :: Module ()
helloPlugin = newModule
  { moduleCmds =
    return
      [ (command "hello") { aliases = ["goodbye"]
                          , help    = say
                            "hello/goodbye <arg>. Simplest possible plugin"
                          , process = \xs -> say ("Hello world. " ++ xs)
                          }
      ]
  }


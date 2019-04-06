
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Lambdabot.Plugin.Misc.Error
  ( errorPlugin
  , failOnLoad
  , errorOnLoad
  )
where

import           Lambdabot.Config               ( config )
import           Lambdabot.Plugin               ( Module
                                                , moduleCmds
                                                , command
                                                , help
                                                , process
                                                , moduleInit
                                                , newModule
                                                , say
                                                , getConfig
                                                )

import           Control.Monad

config "failOnLoad"  [t| Bool |] [| False |]
config "errorOnLoad" [t| Bool |] [| False |]

errorPlugin :: Module ()
errorPlugin = newModule
  { moduleCmds =
    return
      [ (command "error")
        { help    = say "Throw an error, see what lambdabot does with it!"
        , process = error . show
        }
      , (command "fail")
        { help    = say "Throw an error, see what lambdabot does with it!"
        , process = fail . show
        }
      ]
  , moduleInit = do
                   shouldFail <- getConfig failOnLoad
                   when shouldFail (fail "Error module hates the world!")

                   shouldError <- getConfig errorOnLoad
                   when shouldError (error "Error module hates the world!")
  }

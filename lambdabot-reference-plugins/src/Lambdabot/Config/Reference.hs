{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Lambdabot.Config.Reference (
  proxy,
  aspellBinary,
) where

import Lambdabot.Config ( config )
import Network.HTTP.Proxy ( Proxy(NoProxy) )

config "proxy" [t|Proxy|] [|NoProxy|]
config "aspellBinary" [t|String|] [|"aspell"|]

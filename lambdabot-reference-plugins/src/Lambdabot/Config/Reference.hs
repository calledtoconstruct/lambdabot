{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Lambdabot.Config.Reference (
  configAllowRedirects,
  configMaxRedirects,
  configProxy,
  aspellBinary,
) where

import Lambdabot.Config (config)

import Network.HTTP.Proxy (Proxy (NoProxy))

config "configAllowRedirects" [t|Bool|] [|True|]
config "configMaxRedirects" [t|Maybe Int|] [|Just 5|]
config "configProxy" [t|Proxy|] [|NoProxy|]
config "aspellBinary" [t|String|] [|"aspell"|]

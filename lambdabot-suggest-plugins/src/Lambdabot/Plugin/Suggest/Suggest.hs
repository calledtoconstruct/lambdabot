
{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE FlexibleContexts #-}

module Lambdabot.Plugin.Suggest.Suggest (
  suggestPlugin
) where

import Lambdabot.Plugin
import Lambdabot.Util

import Data.Char
import Data.List

type Suggestions  = [String]
type Suggest      = ModuleT Suggestions LB

suggestPlugin :: Module (Suggestions)
suggestPlugin = newModule {
  moduleSerialize = Just stdSerial,
  moduleDefState  = return [],
  moduleInit      = modifyMS (filter (not . null)),
  moduleCmds      = return [
    (command "suggest") {
      help = say "suggest <suggestion> - Add a suggestion to the list.",
      process = addSuggestion . strip isSpace
    },
    (command "suggestions") {
      help = say "suggestions - List the current suggestions.",
      process = const $ listSuggestions
    }
  ]
}

addSuggestion :: String -> Cmd Suggest ()
addSuggestion rest
  | null rest = say "Incorrect arguments to quote"
  | otherwise = do
    withMS $ \bs writer -> writer $ bs ++ rest : []
    say =<< randomSuccessMsg

listSuggestions :: Cmd Suggest ()
listSuggestions = withMS (\suggestions _ -> say $ concat $ ["\""] ++ (intersperse "\", \"" suggestions) ++ ["\""])


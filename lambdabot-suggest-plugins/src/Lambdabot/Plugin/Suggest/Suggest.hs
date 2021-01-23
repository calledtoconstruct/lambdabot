{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternGuards #-}

module Lambdabot.Plugin.Suggest.Suggest (
  suggestPlugin,
) where

import Lambdabot.Plugin (
  Cmd,
  Command (help, process),
  LB,
  Module (moduleCmds, moduleDefState, moduleInit, moduleSerialize),
  ModuleT,
  MonadLBState (withMS),
  command,
  modifyMS,
  newModule,
  say,
  stdSerial,
 )
import Lambdabot.Util (randomSuccessMsg, strip)

import Data.Char (isSpace)

type Suggestions = [String]
type Suggest = ModuleT Suggestions LB

suggestPlugin :: Module Suggestions
suggestPlugin =
  newModule
    { moduleSerialize = Just stdSerial
    , moduleDefState = return []
    , moduleInit = modifyMS (filter (not . null))
    , moduleCmds =
        return
          [ (command "suggest")
              { help = say "suggest <suggestion> - Add a suggestion to the list."
              , process = addSuggestion . strip isSpace
              }
          , (command "suggestions")
              { help = say "suggestions - List the current suggestions."
              , process = const listSuggestions
              }
          , (command "remove-suggestion")
              { help = say "suggestions - List the current suggestions."
              , process = removeSuggestion . strip isSpace
              }
          ]
    }

addSuggestion :: String -> Cmd Suggest ()
addSuggestion rest
  | null rest = say "Incorrect arguments to quote"
  | otherwise = do
    withMS $ \bs writer -> writer $ bs ++ [rest]
    say =<< randomSuccessMsg

listSuggestions :: Cmd Suggest ()
listSuggestions = withMS (\suggestions _ -> mapM_ say $ formatSuggestions suggestions)

formatSuggestions :: [String] -> [String]
formatSuggestions suggestions = map formatSuggestion indexedSuggestions
 where
  indexedSuggestions = zip [1 ..] suggestions

formatSuggestion :: (Int, String) -> String
formatSuggestion (index, suggestion) = show index ++ ". " ++ suggestion

removeSuggestion :: String -> Cmd Suggest ()
removeSuggestion rest
  | null rest = say "Provide index of suggestion to remove."
  | otherwise = do
    let index = read rest
    withMS
      ( \suggestions write -> do
          let updated' = removeSuggestion' suggestions index
          case updated' of
            Just updated -> do
              write updated
              say =<< randomSuccessMsg
            Nothing -> say "Index out of range."
      )

removeSuggestion' :: [String] -> Int -> Maybe [String]
removeSuggestion' suggestions index
  | 0 < index && index < (1 + length suggestions) = Just $ remove suggestions index
  | otherwise = Nothing

remove :: [String] -> Int -> [String]
remove suggestions index = map snd . filter ((/= index) . fst) . zip [1 ..] $ suggestions

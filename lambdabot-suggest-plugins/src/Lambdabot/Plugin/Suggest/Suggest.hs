{-# LANGUAGE OverloadedStrings #-}

module Lambdabot.Plugin.Suggest.Suggest (
  suggestPlugin,
) where

import Lambdabot.Plugin (
  Cmd (),
  Command (help, process),
  LB,
  Module (moduleCmds, moduleDefState, moduleInit, moduleSerialize),
  ModuleT,
  MonadLBState (withMS),
  command,
  getTarget,
  modifyMS,
  nName,
  newModule,
  say,
  stdSerial,
 )
import Lambdabot.Util (randomSuccessMsg)

import Data.List (partition)
import qualified Data.Text as T
import qualified Data.Text.Read as T

type ChannelName = T.Text
type Suggestion = T.Text
type Suggestions = [(ChannelName, [Suggestion])]
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
              , process = addSuggestion . T.strip . T.pack
              }
          , (command "suggestions")
              { help = say "suggestions - List the current suggestions."
              , process = const listSuggestions
              }
          , (command "remove-suggestion")
              { help = say "suggestions - List the current suggestions."
              , process = removeSuggestion . T.strip . T.pack
              }
          ]
    }

addSuggestion :: T.Text -> Cmd Suggest ()
addSuggestion rest
  | T.null rest = say "Incorrect arguments to quote"
  | otherwise = withMS $ \ms writer -> do
    thisChannelName <- T.pack . nName <$> getTarget
    let (thisChannelState, otherChannels) = partition ((== thisChannelName) . fst) ms
    case thisChannelState of
      [] -> writer $ (thisChannelName, [rest]) : otherChannels
      thisChannel : _ -> writer $ (thisChannelName, rest : filter (/= rest) (snd thisChannel)) : otherChannels
    say =<< randomSuccessMsg

listSuggestions :: Cmd Suggest ()
listSuggestions = withMS $ \ms _ -> do
  thisChannelName <- T.pack . nName <$> getTarget
  let thisChannelState = filter ((== thisChannelName) . fst) ms
  case thisChannelState of
    [] -> say "No suggestions have been submitted.  Add one today using ?suggest <idea>"
    thisChannel : _ -> mapM_ (say . T.unpack) $ formatSuggestions $ snd thisChannel

formatSuggestions :: [T.Text] -> [T.Text]
formatSuggestions suggestions = map formatSuggestion indexedSuggestions
 where
  indexedSuggestions = zip [1 ..] suggestions

formatSuggestion :: (Int, T.Text) -> T.Text
formatSuggestion (index, suggestion) = T.pack (show index) `T.append` ". " `T.append` suggestion

removeSuggestion :: T.Text -> Cmd Suggest ()
removeSuggestion rest
  | T.null rest = say "Provide index of suggestion to remove."
  | otherwise = case T.decimal rest of
    Right (index, _) -> withMS $ \ms write -> do
      thisChannelName <- T.pack . nName <$> getTarget
      let (thisChannelState, otherChannels) = partition ((== thisChannelName) . fst) ms
      case thisChannelState of
        [] -> say =<< randomSuccessMsg
        thisChannel : _ ->
          let updated' = removeSuggestion' (snd thisChannel) index
           in case updated' of
                Just updated -> do
                  write $ (thisChannelName, updated) : otherChannels
                  say =<< randomSuccessMsg
                Nothing -> say "Index out of range."
    Left err -> say err

removeSuggestion' :: [T.Text] -> Int -> Maybe [T.Text]
removeSuggestion' suggestions index
  | 0 < index && index < (1 + length suggestions) = Just $ remove suggestions index
  | otherwise = Nothing

remove :: [T.Text] -> Int -> [T.Text]
remove suggestions index = map snd . filter ((/= index) . fst) . zip [1 ..] $ suggestions

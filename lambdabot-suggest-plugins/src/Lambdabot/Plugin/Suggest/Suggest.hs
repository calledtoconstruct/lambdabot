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
import Lambdabot.Util (randomSuccessMsg, strip)

import Data.Char (isSpace)
import Data.List (partition)

type ChannelName = String
type Suggestion = String
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
  | otherwise = withMS $ \ms writer -> do
    thisChannelName <- nName <$> getTarget
    let (thisChannelState, otherChannels) = partition ((== thisChannelName) . fst) ms
    case thisChannelState of
      [] -> writer $ (thisChannelName, [rest]) : otherChannels
      thisChannel : _ -> writer $ (thisChannelName, rest : filter (/= rest) (snd thisChannel)) : otherChannels
    say =<< randomSuccessMsg

listSuggestions :: Cmd Suggest ()
listSuggestions = withMS $ \ms _ -> do
  thisChannelName <- nName <$> getTarget
  let thisChannelState = filter ((== thisChannelName) . fst) ms
  case thisChannelState of
    [] -> say "No suggestions have been submitted.  Add one today using ?suggest <idea>"
    thisChannel : _ -> mapM_ say $ formatSuggestions $ snd thisChannel

formatSuggestions :: [String] -> [String]
formatSuggestions suggestions = map formatSuggestion indexedSuggestions
 where
  indexedSuggestions = zip [1 ..] suggestions

formatSuggestion :: (Int, String) -> String
formatSuggestion (index, suggestion) = show index ++ ". " ++ suggestion

removeSuggestion :: String -> Cmd Suggest ()
removeSuggestion rest
  | null rest = say "Provide index of suggestion to remove."
  | otherwise =
    let index = read rest
     in withMS $ \ms write -> do
          thisChannelName <- nName <$> getTarget
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

removeSuggestion' :: [String] -> Int -> Maybe [String]
removeSuggestion' suggestions index
  | 0 < index && index < (1 + length suggestions) = Just $ remove suggestions index
  | otherwise = Nothing

remove :: [String] -> Int -> [String]
remove suggestions index = map snd . filter ((/= index) . fst) . zip [1 ..] $ suggestions

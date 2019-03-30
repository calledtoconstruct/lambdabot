
{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE FlexibleContexts #-}

module Lambdabot.Plugin.Points.Points (
  pointsPlugin
) where

import Lambdabot.Plugin
import Lambdabot.Util

import Control.Monad
import Data.Char
import Data.Maybe

type Suggestions  = [String]
type Suggest      = ModuleT Suggestions LB

pointsPlugin :: Module (Suggestions)
pointsPlugin = newModule {
  moduleSerialize = Just stdSerial,
  moduleDefState  = return [],
  moduleInit      = modifyMS (filter (not . null)),
  moduleCmds      = return [
    (command "points") {
      help = say "points - Shows how many points you have.",
      process = \_ -> say "Coming soon!"
    },
    (command "leaderboard") {
      help = say "leaderboard - List the top ten from the leaderboard.",
      process = \_ -> say "Coming soon!"
    },
    (command "give-points") {
      help = say "give-points [who] [number] - Give some of your points to someone.",
      process = \_ -> say "Coming soon!"
    },
    (command "leaderboard-all") {
      privileged = True,
      help = say "leaderboard-all - List the entire leaderboard.",
      process = \_ -> say "Coming soon!"
    },
    (command "gift-points") {
      privileged = True,
      help = say "gift-points [who] [number] - Gift some points to someone.",
      process = \_ -> say "Coming soon!"
    },
    (command "charge-points") {
      privileged = True,
      help = say "charge-points [who] [number] - Subtract some points from someone.",
      process = \_ -> say "Coming soon!"
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
listSuggestions = withMS (\suggestions _ -> mapM_ say $ formatSuggestions suggestions)

formatSuggestions :: [String] -> [String]
formatSuggestions suggestions = map formatSuggestion indexedSuggestions
  where indexedSuggestions = zip [1..] suggestions

formatSuggestion :: (Int, String) -> String
formatSuggestion (index, suggestion) = show index ++ ". " ++ suggestion

removeSuggestion :: String -> Cmd Suggest ()
removeSuggestion rest
  | null rest = say "Provide index of suggestion to remove."
  | otherwise = do
    let index = read rest
    withMS (\suggestions write -> do
      let updated' = removeSuggestion' suggestions index
      case updated' of
        Just updated -> do
          write updated
          say =<< randomSuccessMsg
        Nothing -> say "Index out of range.")

removeSuggestion' :: [String] -> Int -> Maybe [String]
removeSuggestion' suggestions index
  | 0 < index && index < (1 + length suggestions) = Just $ remove suggestions index
  | otherwise = Nothing

remove :: [String] -> Int -> [String]
remove suggestions index = map snd . filter ((/= index) . fst) . zip [1..] $ suggestions


{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Lambdabot.Plugin.Points.Points (
  pointsPlugin
) where

import Lambdabot.Plugin
import Lambdabot.Compat.PackedNick (packNick, unpackNick)

import qualified Data.ByteString.Char8 as P
import Control.Monad
import Data.Maybe
import Data.List.Split
import Data.ByteString.Lazy (fromChunks, toChunks)
import Codec.Compression.GZip

gzip   :: P.ByteString -> P.ByteString
gzip   = P.concat . toChunks . compress . fromChunks . (:[])

gunzip :: P.ByteString -> P.ByteString
gunzip = P.concat . toChunks . decompress . fromChunks . (:[])

type PointRecord = (P.ByteString, Int)

type PointsState = [PointRecord]
type Points = ModuleT PointsState LB
-- type PointsStateTransform = (PointsState -> PointsState)
-- type PointsStateTransformResult = (Maybe PointsState, String)

instance Packable (PointsState) where
  readPacked ps = readPackedEntry (splitAt 2) buildPointRecord (P.lines . gunzip $ ps)
  showPacked = gzip . P.unlines . concatMap (\(who, points) -> [who, P.pack . show $ points])

buildPointRecord :: [P.ByteString] -> PointRecord
buildPointRecord (who: points: _) = (who, read . P.unpack $ points)

pointsStatePackedSerial :: Serial (PointsState)
pointsStatePackedSerial = Serial (Just . showPacked) (Just . readPacked)

pointsPlugin :: Module (PointsState)
pointsPlugin = newModule {
  moduleSerialize = Just stdSerial,
  moduleDefState  = return [],
  moduleInit      = modifyMS (filter (not . null)),
  moduleCmds      = return [
    (command "points") {
      help = say "points - Shows how many points you have.",
      process = \rest -> showPoints rest
    },
    (command "leaderboard") {
      help = say "leaderboard - List the top ten from the leaderboard.",
      process = \rest -> showPoints rest
    },
    (command "give-points") {
      help = say "give-points [who] [number] - Give some of your points to someone.",
      process = \rest -> showPoints rest
    },
    (command "leaderboard-all") {
      privileged = True,
      help = say "leaderboard-all - List the entire leaderboard.",
      process = \rest -> showPoints rest
    },
    (command "gift-points") {
      privileged = True,
      help = say "gift-points [who] [number] - Gift some points to someone.",
      process = \rest -> giftPoints rest
    },
    (command "charge-points") {
      privileged = True,
      help = say "charge-points [who] [number] - Subtract some points from someone.",
      process = \rest -> showPoints rest
    }
  ]
}

showPoints :: String -> Cmd Points ()
showPoints []   = do
  sender <- fmap packNick getSender
  withMS $ \pointsState _ -> do
    who <- showNick $ unpackNick sender
    let (found, _) = find pointsState who
    case found of
      Just (_, score) -> say $ "You have " ++ show score ++ " points."
      Nothing         -> say "You have 0 points."
showPoints rest = case splitOn " " rest of
  who: [] -> do
    withMS $ \pointsState _ -> do
      let (found, _) = find pointsState who
      case found of
        Just (_, score) -> say $ who ++ " has " ++ show score ++ " points."
        Nothing         -> say $ who ++ " has 0 points."

giftPoints :: String -> Cmd Points ()
giftPoints rest = case splitOn " " rest of
  who: points: [] -> do
    withMS $ \pointsState writer -> do
      let (nextState, message) = insertOrAddPoints pointsState who points
      when (isJust nextState) $ writer $ fromJust nextState
      say message
  _ -> say "Too few arguments, please include who and how many points"

find :: PointsState -> String -> (Maybe PointRecord, PointsState)
find (current: rest) who
  | (P.unpack $ fst current) == who = (Just current, rest)
  | otherwise                       = do
    let (found, list) = find rest who
    (found, current: list)
find [] _ = (Nothing, [])

insertOrAddPoints :: PointsState -> String -> String -> (Maybe PointsState, String)
insertOrAddPoints list who pointsString = do
  let points = read pointsString
  let (found, others) = find list who
  case found of
    Just (name, score)  -> (Just $ (name, score + points): others, "Updated.")
    Nothing             -> (Just $ (P.pack who, points): list, "Added.")
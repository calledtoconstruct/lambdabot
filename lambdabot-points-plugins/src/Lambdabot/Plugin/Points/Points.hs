
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Lambdabot.Plugin.Points.Points (
  pointsPlugin
) where

import Lambdabot.Plugin (
  Module,
  ModuleT,
  LB,
  Cmd,
  Packable ( readPacked, showPacked ),
  Serial (..),
  moduleDefState,
  moduleInit
  , help
  , moduleCmds
  , moduleSerialize
  , process
  , aliases
  , privileged
  , command
  , say
  , readMS
  , showNick
  , getSender
  , modifyMS
  , readM
  , withMS,
  stdSerial,
  newModule )
import Lambdabot.Util ( readPackedEntry )
import Lambdabot.Compat.PackedNick (packNick, unpackNick)

import qualified Data.ByteString.Char8 as P
import Data.List (sortOn)
import Data.List.Split -- (splitOn)
import Data.Ord
import Data.ByteString.Lazy (fromChunks, toChunks)
import Codec.Compression.GZip
import Text.Read (readMaybe)

gzip   :: P.ByteString -> P.ByteString
gzip   = P.concat . toChunks . compress . fromChunks . (:[])

gunzip :: P.ByteString -> P.ByteString
gunzip = P.concat . toChunks . decompress . fromChunks . (:[])

type PointRecord = (P.ByteString, Int)

type PointsState = [PointRecord]
type Points = ModuleT PointsState LB
-- type PointsStateTransform = (PointsState -> PointsState)
-- type PointsStateTransformResult = (Maybe PointsState, String)

instance Packable PointsState where
  readPacked ps = readPackedEntry (splitAt 2) buildPointRecord (P.lines . gunzip $ ps)
  showPacked = gzip . P.unlines . concatMap (\(who, points) -> [who, P.pack . show $ points])

buildPointRecord :: [P.ByteString] -> PointRecord
buildPointRecord (who: points: _) = (who, read . P.unpack $ points)

pointsStatePackedSerial :: Serial PointsState
pointsStatePackedSerial = Serial (Just . showPacked) (Just . readPacked)

pointsPlugin :: Module PointsState
pointsPlugin = newModule {
  moduleSerialize = Just stdSerial,
  moduleDefState  = return [],
  moduleInit      = modifyMS (filter (not . null)),
  moduleCmds      = return [
    (command "points") {
      help = say "points - Shows how many points you have.",
      process = showPoints
    },
    (command "leaderboard") {
      help = say "leaderboard - List the top ten from the leaderboard.",
      process = showLeaderboard
    },
    (command "give-points") {
      help = say "give-points [who] [number] - Give some of your points to someone.",
      process = givePoints
    },
    -- Implement this as offline only command.
    -- (command "leaderboard-all") {
    --   privileged = True,
    --   help = say "leaderboard-all - List the entire leaderboard.",
    --   process = \rest -> showPoints rest
    -- },
    (command "gift-points") {
      privileged = True,
      help = say "gift-points [who] [number] - Gift some points to someone.",
      process = giftPoints
    },
    (command "charge-points") {
      privileged = True,
      help = say "charge-points [who] [number] - Subtract some points from someone.",
      process = chargePoints
    }
  ]
}

incorrectArgumentsForWhoAndHowManyPoints :: String
incorrectArgumentsForWhoAndHowManyPoints = "Incorrect number of arguments, please provide who and how many points."

giftPoints :: String -> Cmd Points ()
giftPoints [] = say incorrectArgumentsForWhoAndHowManyPoints
giftPoints rest = case splitOn " " rest of
  [receiver, pointsString] ->
    case readMaybe pointsString of
      Just points -> do
        let absolutePoints = abs points
        withMS $ \initialState writer -> do
          let (finalState, total) = insertOrAddPoints initialState receiver absolutePoints
          writer finalState
          say $ receiver ++ " has been gifted " ++ show absolutePoints ++ " points and now has " ++ show total
      Nothing     -> say "Invalid number of points, please provide who and how many points."
  _ -> say incorrectArgumentsForWhoAndHowManyPoints

chargePoints :: String -> Cmd Points ()
chargePoints rest = case splitOn " " rest of
  [giver, pointsString] -> do
    sender <- fmap packNick getSender
    who <- showNick $ unpackNick sender
    case readMaybe pointsString of
      Just points -> do
        let absolutePoints = negate $ abs points
        withMS $ \initialState writer -> do
          let nextState = updatePoints initialState giver absolutePoints
          case nextState of
            Right _                   -> say $ who ++ ", " ++ giver ++ " does not have " ++ show absolutePoints ++ "."
            Left (finalState, total)  -> do
              writer finalState
              say $ giver ++ " has been charged " ++ show (abs absolutePoints) ++ " points and now has " ++ show total
      Nothing     -> say "Invalid number of points, please provide who and how many points."
  _ -> say incorrectArgumentsForWhoAndHowManyPoints

showLeaderboard :: String -> Cmd Points ()
showLeaderboard _ = withMS $ \pointsState _ -> do
  let orderedList = sortOn (Down . snd) pointsState
  let topTen = take 10 orderedList
  let topTenList = map formatPointRecord $ zip [1..] topTen
  let messages = return $ "Top-ten Leaderboard": topTenList
  mapM_ say =<< messages

formatPointRecord :: (Int, PointRecord) -> String
formatPointRecord (rank, (name, points)) = do
  let who = P.unpack name
  show rank ++ ". " ++ who ++ " with " ++ show points ++ " points."

showPoints :: String -> Cmd Points ()
showPoints []   = do
  sender <- fmap packNick getSender
  who <- showNick $ unpackNick sender
  withMS $ \pointsState _ -> do
    let (found, _) = find pointsState who
    case found of
      Just (_, score) -> say $ who ++ ", You have " ++ show score ++ " points."
      Nothing         -> say $ who ++ ", You have 0 points."
showPoints rest = case splitOn " " rest of
  [who] -> withMS $ \pointsState _ -> do
    let (found, _) = find pointsState who
    case found of
      Just (_, score) -> say $ who ++ " has " ++ show score ++ " points."
      Nothing         -> say $ who ++ " has 0 points."
  _ -> say "Invalid number of arguments, please provide who to show or no arguments."

givePoints :: String -> Cmd Points()
givePoints rest = do
  sender <- fmap packNick getSender
  giver <- showNick $ unpackNick sender
  case splitOn " " rest of
    [receiver, pointsString] ->
      case readMaybe pointsString of
        Just points -> do
          let positivePoints = abs points
          withMS $ \initialState writer -> do
            let (addState, receiverPoints) = insertOrAddPoints initialState receiver positivePoints
            let subtractState = updatePoints addState giver $ negate positivePoints
            case subtractState of
              Right _               -> say $ giver ++ ", You do not have enough to give " ++ show positivePoints ++ " points!"
              Left (finalState, _)  -> do
                writer finalState
                say $ giver ++ " gave " ++ show positivePoints ++ " points to " ++ receiver ++ " who now has " ++ show receiverPoints ++ " points."
        Nothing     -> say "Invalid number of points, please provide who and how many points."
    _ -> say "Too few arguments, please include who and how many points"

find :: PointsState -> String -> (Maybe PointRecord, PointsState)
find (current: rest) who
  | P.unpack (fst current) == who = (Just current, rest)
  | otherwise                       = do
    let (found, list) = find rest who
    (found, current: list)
find [] _ = (Nothing, [])

insertOrAddPoints :: PointsState -> String -> Int -> (PointsState, Int)
insertOrAddPoints list who points = do
  let (found, others) = find list who
  case found of
    Just (name, score)  -> ((name, score + points): others, score + points)
    Nothing             -> ((P.pack who, points): list, points)

updatePoints :: PointsState -> String -> Int -> Either (PointsState, Int) ()
updatePoints list who points = do
  let (found, others) = find list who
  case found of
    Just (name, score)  -> do
      let finalScore = score + points
      if 0 <= finalScore then Left ((name, finalScore): others, finalScore) else notEnoughPointsMessage
    Nothing             -> notEnoughPointsMessage
  where notEnoughPointsMessage = Right ()

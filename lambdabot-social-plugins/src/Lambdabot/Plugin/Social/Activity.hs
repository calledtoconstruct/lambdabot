-- | Logging an IRC channel..
module Lambdabot.Plugin.Social.Activity (activityPlugin) where

import Lambdabot.Monad (registerOutputFilter)
import Lambdabot.Plugin (
  Cmd,
  Command (help, privileged, process),
  LB,
  Module (moduleCmds, moduleDefState, moduleInit),
  ModuleT,
  MonadLBState (withMS),
  Nick (nName),
  command,
  newModule,
  readM,
  readMS,
  readNick,
  say,
  showNick,
 )
import Lambdabot.Util (io)

import Control.Arrow ((&&&))
import Control.Exception (evaluate)
import Data.List (group, isPrefixOf, sort, sortOn)
import Data.Maybe (fromMaybe)
import Data.Ord (Down (Down))
import Data.Time (UTCTime, addUTCTime, getCurrentTime)

type ActivityState = [(UTCTime, Nick)]
type Activity = ModuleT ActivityState LB

activityPlugin :: Module [(UTCTime, Nick)]
activityPlugin =
  newModule
    { moduleDefState = return []
    , moduleInit = registerOutputFilter activityFilter
    , moduleCmds =
        return
          [ (command "activity")
              { help = say helpStr
              , process = activity False
              }
          , (command "activity-full")
              { help = say helpStr
              , privileged = True
              , process = activity True
              }
          ]
    }

helpStr :: String
helpStr = "activity seconds. Find out where/how much the bot is being used"

activity :: Bool -> String -> Cmd Activity ()
activity full args = do
  let obscure nm
        | full || isPrefixOf "#" (nName nm) = return nm
        | otherwise = readNick "private"

  now <- io getCurrentTime
  let cutoff = addUTCTime (- fromInteger (fromMaybe 90 $ readM args)) now
  users <- mapM (obscure . snd) . takeWhile ((> cutoff) . fst) =<< readMS
  let agg_users = sortOn Data.Ord.Down . map (length &&& head) . group . sort $ users
  fmt_agg <-
    fmap
      (unwords . (:) (show (length users) ++ "*total"))
      (mapM (\(n, u) -> do u' <- showNick u; return (show n ++ "*" ++ u')) agg_users)

  say fmt_agg

activityFilter :: Nick -> [String] -> Activity [String]
activityFilter target lns = do
  io $ evaluate $ foldr (seq . foldr seq ()) () lns
  withMS $ \st wr -> do
    now <- io getCurrentTime
    wr (map (const (now, target)) lns ++ st)
  return lns

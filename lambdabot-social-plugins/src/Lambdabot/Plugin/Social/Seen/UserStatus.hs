module Lambdabot.Plugin.Social.Seen.UserStatus where

import Lambdabot.Compat.AltTime (ClockTime, TimeDiff, addToClockTime, diffClockTimes)
import Lambdabot.Compat.PackedNick (PackedNick)
import Lambdabot.Plugin.Social.Seen.StopWatch (StopWatch, stopWatch)

import Data.Binary (Binary (get, put), getWord8, putWord8)
import qualified Data.ByteString as BS
import Data.List (nub)

-- | The type of channels
type Channel = BS.ByteString

{- | We last heard the user speak at ClockTime; since then we have missed
   TimeDiff of him because we were absent.
-}
type LastSpoke = Maybe (ClockTime, TimeDiff)

-- | 'UserStatus' keeps track of the status of a given Nick name.
data UserStatus
  = -- | Records when the nick last spoke and that the nick is currently
    --   in [Channel].
    Present !LastSpoke [Channel]
  | -- | The nick is not present and was last seen at ClockTime in Channel.
    --   The second argument records how much we've missed.
    NotPresent !ClockTime !StopWatch [Channel]
  | -- | The bot parted a channel where the user was. The Clocktime
    --   records the time and Channel the channel this happened in.
    --   We also save the reliablility of our information and the
    --   time we last heard the user speak.
    WasPresent !ClockTime !StopWatch !LastSpoke [Channel]
  | -- | The user changed nick to something new.
    NewNick !PackedNick
  deriving (Show, Read)

instance Binary UserStatus where
  put (Present sp ch) = putWord8 0 >> put sp >> put ch
  put (NotPresent ct sw ch) = putWord8 1 >> put ct >> put sw >> put ch
  put (WasPresent ct sw sp ch) = putWord8 2 >> put ct >> put sw >> put sp >> put ch
  put (NewNick n) = putWord8 3 >> put n

  get =
    getWord8 >>= \h -> case h of
      0 -> Present <$> get <*> get
      1 -> NotPresent <$> get <*> get <*> get
      2 -> WasPresent <$> get <*> get <*> get <*> get
      3 -> NewNick <$> get
      _ -> error "Seen.UserStatus.get"

-- | Update the user status when a user joins a channel.
updateJ ::
  -- | If the bot joined the channel, the time that
  --   happened, i.e. now.
  Maybe ClockTime ->
  -- | The channels the user joined.
  [Channel] ->
  -- | The old status
  UserStatus ->
  -- | The new status
  -- The user was present before, so he's present now.
  UserStatus
updateJ _ c (Present ct cs) = Present ct $ nub (c ++ cs)
-- The user was present when we left that channel and now we've come back.
-- We need to update the time we've missed.
updateJ (Just now) cs (WasPresent lastSeen _ (Just (lastSpoke, missed)) channels)
  | head channels `elem` cs =
    ---                 newMissed
    --- |---------------------------------------|
    --- |-------------------|                   |
    ---        missed    lastSeen              now
    let newMissed = addToClockTime missed now `diffClockTimes` lastSeen
     in newMissed `seq` Present (Just (lastSpoke, newMissed)) cs
-- Otherwise, we create a new record of the user.
updateJ _ cs _ = Present Nothing cs

{- | Update a user who is not present. We just convert absolute missing time
   into relative time (i.e. start the "watch").
-}
updateNP :: ClockTime -> Channel -> UserStatus -> UserStatus
updateNP now _ (NotPresent ct missed c) =
  NotPresent ct (stopWatch now missed) c
-- The user might be gone, thus it's meaningless when we last heard him speak.
updateNP now chan (WasPresent lastSeen missed _ cs)
  | head cs == chan = WasPresent lastSeen (stopWatch now missed) Nothing cs
updateNP _ _ status = status

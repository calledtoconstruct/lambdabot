module Lambdabot.Plugin.Story.Story (storyPlugin) where

import Lambdabot.Config.Story (defaultStories, secondsToWaitBeforeMovingToTheNextWord)
import Lambdabot.IRC (IrcMessage (IrcMessage), MessageDirection (Outbound), ircDirection, ircMsgCommand, ircMsgLBName, ircMsgParams, ircMsgPrefix, ircMsgServer, ircTags)
import Lambdabot.Plugin (
  Cmd,
  LB,
  Module,
  ModuleT,
  MonadConfig (getConfig),
  aliases,
  command,
  contextual,
  help,
  io,
  moduleCmds,
  moduleDefState,
  moduleInit,
  moduleSerialize,
  nName,
  nTag,
  newModule,
  privileged,
  process,
  randomElem,
  randomSuccessMsg,
  say,
  send,
  stdSerial,
  withMS,
  withMsg,
 )
import Lambdabot.Plugin.Story.Configuration (Definition, GameState (..), StoryState (..), newGameState, newStoryState, Vote)

import Control.Concurrent.Lifted (fork, threadDelay)
import Control.Monad.Reader (MonadTrans (lift), unless, void, when)
import Data.Char (isAlpha, isNumber)
import Data.Either (fromLeft)
import Data.List (findIndex, isPrefixOf, partition)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Lambdabot.Logging (debugM)
import qualified Lambdabot.Message as Msg
import Text.ParserCombinators.ReadP (ReadP, between, munch, munch1, readP_to_S, sepBy)

type Story = ModuleT StoryState LB

storyPlugin :: Module StoryState
storyPlugin =
  newModule
    { moduleSerialize = Just stdSerial
    , moduleDefState = do
        initialStories <- getConfig defaultStories
        newStoryState initialStories
    , moduleInit = return ()
    , moduleCmds =
        return
          [ (command "story")
              { help = say "story - Start a random story."
              , aliases = ["story-time"]
              , process = commandStartStory
              , privileged = True
              }
          , (command "add-story")
              { help = say "add-story <story> <definition> - Add a new story to the database."
              , process = commandAddStory
              , privileged = True
              }
          , (command "remove-story")
              { aliases = ["rm-story"]
              , help = say "remove-story <story> - Remove a story from the database."
              , process = commandRemoveStory
              , privileged = True
              }
          , (command "reset-story")
              { aliases = ["rst-story"]
              , help = say "reset-story - Reset active story."
              , process = commandResetStory
              , privileged = True
              }
          , (command "factory-reset-story")
              { help = say "factory-reset-story - Reset entire story state to factory defaults."
              , process = commandResetFactoryDefaultStory
              , privileged = True
              }
          ]
    , contextual = commandContextual
    }

-- TODO: Fork a background thread here?
commandContextual :: String -> Cmd Story ()
commandContextual txt =
  let wrds = words txt
      oneWord = length wrds == 1
   in when oneWord $
        withMsg $ \msg ->
          let srvr = Msg.server msg
              chan = head $ Msg.channels msg
              channelName = srvr ++ nTag chan ++ nName chan
           in withMS $ \storyState writer ->
                let (game, other) = extractGame channelName storyState
                 in unless (null game) $
                      writer
                        storyState
                          { games = addVote (head wrds) (head game) : other
                          }

-- check that the word exists in the wordnet database
-- determine the type of the word
-- if the word is the same type as the next word in the story

addVote :: String -> (String, GameState) -> (String, GameState)
addVote word (channelName, gameState) =
  let (same, other) = partition ((==) word . fst) $ votes gameState
      newScore =
        if null same
          then [(word, 1)]
          else [(word, 1 + snd (head same))]
   in (channelName, gameState{votes = newScore ++ other})

-- | Extract the game for the current channel
-- >>> ss <- newStoryState []
-- >>> let gs = MkGameState { server = "srvr", channel = "chnl", name = "name", botName = "btnm", story = ("title", "stry"), votes = [] }
-- >>> let (game, other) = extractGame "def" ss { games = [("abc", gs), ("def", gs)]}
-- >>> (not $ null game) && ("def" == fst (head game)) && (not $ null other) && ("abc" == fst (head other))
-- True
-- 
extractGame :: String -> StoryState -> ([(String, GameState)], [(String, GameState)])
extractGame channelName = partition ((==) channelName . fst) . games

commandStartStory :: String -> Cmd Story ()
commandStartStory _ = withMsg $ \msg ->
  let srvr = Msg.server msg
      chan = head $ Msg.channels msg
      botn = Msg.lambdabotName msg
   in withMS $ \storyState writer ->
        let channelName = srvr ++ nTag chan ++ nName chan
            (game, other) = extractGame channelName storyState
         in when (null game) $ do
              delay <- getConfig secondsToWaitBeforeMovingToTheNextWord
              sts <- io $ randomElem $ stories storyState
              let newGame = newGameState srvr chan botn sts
              writer storyState{games = (channelName, newGame) : other}
              say $ "Let's make a story together, the title will be '" ++ fst sts ++ "'."
              say $ nextNeed $ fromLeft ("", "") $ nextWord newGame
              void $ lift $ fork $ storyLoop delay channelName

-- | Find the next placeholder or return the completed story
-- >>> nextWord MkGameState {story = ("title", "This is a __story__ with __two__ placeholders.")}
-- Left ("story","")
--
-- >>> nextWord MkGameState {story = ("title", "This is a story with zero placeholders.")}
-- Right "This is a story with zero placeholders."
--
nextWord :: GameState -> Either (String, String) String
nextWord gameState =
  let stry = story gameState
      text = snd stry
      wrds = words text
      fillIn = filter ("__" `isPrefixOf`) wrds
   in if null fillIn
        then Right text
        else
          let parts = splitOn ":" $ init $ init $ tail $ tail $ head fillIn
           in if length parts == 1 then Left (head parts, []) else Left (head parts, last parts)

nextNeed :: (String, String) -> String
nextNeed (wordType, wordSubType) =
  let subType = if not $ null wordSubType then ", specifically a " ++ wordSubType else ""
   in "I need a " ++ wordType ++ subType ++ ", type your choice now..."

highestVote :: (String, String) -> (String, Int) -> String
highestVote (wordType, wordSubType) (wrd, num) =
  let subType = if not $ null wordSubType then " (" ++ wordSubType ++ ")" else ""
   in "The most popular " ++ wordType ++ subType ++ " was '" ++ wrd ++ "' with " ++ show num ++ " votes."

storyLoop :: Int -> String -> Story ()
storyLoop delayInSeconds channelName = do
  void $ io $ threadDelay $ delayInSeconds * 1000 * 1000
  withMS $ \storyState writer -> do
    let (game, other) = extractGame channelName storyState
    unless (null game) $
      let gameState = snd $ head game
          title = fst $ story gameState
       in case nextWord gameState of
            Left needed -> do
              let mostVoted = mostVotes $ votes gameState
              let updatedGameState = gameState{votes = [], story = replace mostVoted $ story gameState}
              writer $
                storyState
                  { games = (channelName, updatedGameState) : other
                  }
              lift $ sendText gameState $ highestVote needed mostVoted
              case nextWord updatedGameState of
                Left need -> do
                  lift $ sendText gameState $ nextNeed need
                  void $ fork $ storyLoop delayInSeconds channelName
                Right _ -> do
                  lift $ sendText gameState "All done! Sending the story to the publisher! Get ready..."
                  void $ fork $ storyLoop 5 channelName
            Right stry -> do
              lift $ sendText gameState $ "This is the story we made!  Title: '" ++ title ++ "' Story: " ++ stry
              writer $
                storyState
                  { games = other
                  }

-- | Select the vote with the highest rating
-- >>> mostVotes [("abc", 2), ("def", 3), ("ghi", 1)]
-- ("def",3)
--
mostVotes :: [Vote] -> Vote
mostVotes = foldr (\l r -> if snd l > snd r then l else r) ("", 0)

-- | Replace a placeholder with the word which received the most votes
-- | Note, this fails if no placeholder exists.
-- >>> replace ("word", 1) ("title", "This is the __story__ text.")
-- ("title","This is the word text.")
--
replace :: Vote -> Definition -> Definition
replace (wrd, _) (ttl, stry) =
  let wrds = words stry
      location = fromJust $ findIndex ("__" `isPrefixOf`) wrds
      (before, after) = splitAt location wrds
      replaced = unwords $ before ++ [wrd] ++ tail after
   in (ttl, replaced)

sendText :: GameState -> String -> LB ()
sendText gameState text = do
  send
    IrcMessage
      { ircMsgServer = server gameState
      , ircMsgLBName = name gameState
      , ircMsgPrefix = botName gameState ++ "!n=" ++ botName gameState ++ "@" ++ botName gameState ++ ".tmi.twitch.tv"
      , ircMsgCommand = "PRIVMSG"
      , ircMsgParams = [channel gameState, ":" ++ text]
      , ircDirection = Outbound
      , ircTags = []
      }

-- | Parser tests
-- >>> fst $ last $ readP_to_S storyParameterParser "\"title\" \"this is the story.\""
-- ["title","this is the story."]
--
-- >>> fst $ last $ readP_to_S storyParameterParser "\"title\" this is the story."
-- ["title","this is the story."]
--
storyParameterParser :: ReadP [String]
storyParameterParser = storyPartParser `sepBy` storySpaceParser

storySpaceParser :: ReadP String
storySpaceParser = munch1 (' ' ==)

-- | Quoted string
-- >>> fst $ last $ readP_to_S storyPartParser "\"some text goes here\""
-- "some text goes here"
--
-- >>> fst $ last $ readP_to_S storyPartParser "some text goes here"
-- "some text goes here"
--
storyPartParser :: ReadP String
storyPartParser = between storyQuoteParser storyQuoteParser storyTokenParser

storyTokenParser :: ReadP String
storyTokenParser = munch1 (\c -> isAlpha c || isNumber c || elem c " !@#$%^&*()-_=+\'\\|;:,<.>[]{}/?")

storyQuoteParser :: ReadP String
storyQuoteParser = munch ('"' ==)

commandAddStory :: String -> Cmd Story ()
commandAddStory msg =
  let parts = fst $ last $ readP_to_S storyParameterParser msg
      twoParts = length parts == 2
      titleExists = not $ null $ head parts
      storyExists = not $ null $ last parts
   in if twoParts && titleExists && storyExists
        then withMS $ \storyState writer -> do
          let title = head parts
          let stry = last parts
          let newStory = (title, stry)
          let otherStories = filter ((/=) title . fst) $ stories storyState
          writer storyState{stories = newStory : otherStories}
          confirmation <- randomSuccessMsg
          say confirmation
        else do
          debugM $ show parts
          say "Incorrect parameters, please provide a title and a story both enclosed in double-quotes and separated by a space."

commandRemoveStory :: String -> Cmd Story ()
commandRemoveStory msg =
  let title = fst $ last $ readP_to_S storyPartParser msg
   in if not $ null title
        then withMS $ \storyState writer ->
          let sts = stories storyState
              (_, other) = partition ((==) title . fst) sts
           in if length other /= length sts
                then do
                  writer storyState{stories = other}
                  confirmation <- randomSuccessMsg
                  say confirmation
                else say "I couldn't find a story with that title."
        else say "Incorrect parameters, please provide a title enclosed in double-quotes."

commandResetStory :: String -> Cmd Story ()
commandResetStory resetAll = withMsg $ \msg ->
  let srvr = Msg.server msg
      chan = head $ Msg.channels msg
   in withMS $ \storyState writer -> do
        let channelName = srvr ++ nTag chan ++ nName chan
        let others = filter ((/=) channelName . fst) $ games storyState
        writer storyState{games = if resetAll == "all" then [] else others}
        confirmation <- randomSuccessMsg
        say confirmation

commandResetFactoryDefaultStory :: String -> Cmd Story ()
commandResetFactoryDefaultStory _ = withMS $ \_ writer -> do
  ds <- getConfig defaultStories
  nss <- newStoryState ds
  writer nss
  confirmation <- randomSuccessMsg
  say confirmation

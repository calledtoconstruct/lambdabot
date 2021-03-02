{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Lambdabot.Plugin.Hangman.Game (
  Result (Result),
  game,
  messages,
  Game (NoGame, InGame),
  Hangman,
  HangmanState,
  newGame,
  getConfiguration,
  initializeGame,
  showGame,
  showBoard,
  showGuesses,
  calculateRemaining,
  GameState (..),
  Messages,
  gameInProgress,
  noGameInProgress,
  incorrectArgumentsForStart,
  incorrectArgumentsForShow,
  incorrectArgumentsForProgress,
  incorrectArgumentsForAppend,
  incorrectArgumentsForAddPhrase,
  incorrectArgumentsForRemovePhrase,
  messageIncorrectArgumentsForConfigure,
  messageUnknownConfigurationOption,
  messageConfigurationUpdated,
  substituteTokens,
  incorrectArgumentsForFinalAnswer,
  hist,
  most,
  getMostPopularGuess,
  isSolved,
  completeSetOfLetters,
) where

import Lambdabot.Nick (Nick, fmtNick)
import Lambdabot.Plugin.Hangman.Configuration (
  Configuration (
    defaultSecondsBetweenCycles,
    defaultWarningAt,
    initialAllowedMisses,
    messageGuessed,
    messageGuessing,
    messageIncorrectGuessesTried,
    messageNewGameHasBegun,
    messageNumberOfGuessesRemaining
  ),
  Message,
  Phrase,
  elemText,
  selectPhrase,
 )

import qualified Data.Map as M
import qualified Data.Text as T
import Data.Universe.Helpers ((+++))
import GHC.Generics (Generic)
import Lambdabot.Module (LB)
import Lambdabot.Plugin (ModuleT)

type HangmanState = [Game]
type Hangman = ModuleT HangmanState LB

data Game
  = NoGame Configuration
  | InGame GameState Configuration
  deriving (Generic, Show, Read)

data GameState = GameState
  { userLetters :: Phrase
  , correctLetters :: Phrase
  , incorrectLetters :: Phrase
  , allowedMisses :: Int
  , secondsToNextCycle :: Int
  , warningAt :: [Int]
  , target :: Phrase
  , server :: String
  , channel :: String
  , initiator :: String
  , botName :: String
  }
  deriving (Generic, Show, Read)

type Messages = [Message]

data Result = Result
  { messages :: Messages
  , game :: Game
  }

newGame :: Phrase -> Configuration -> String -> Nick -> Nick -> Nick -> Game
newGame answer configuration serverName channelName userName lambdabotName =
  InGame
    ( GameState
        { target = answer
        , userLetters = ""
        , correctLetters = ""
        , incorrectLetters = ""
        , secondsToNextCycle = defaultSecondsBetweenCycles configuration
        , warningAt = defaultWarningAt configuration
        , allowedMisses = initialAllowedMisses configuration
        , server = serverName
        , channel = fmtNick serverName channelName
        , initiator = fmtNick serverName userName
        , botName = fmtNick serverName lambdabotName
        }
    )
    configuration

getConfiguration :: Game -> Configuration
getConfiguration (NoGame configuration) = configuration
getConfiguration (InGame _ configuration) = configuration

initializeGame :: Game -> String -> Nick -> Nick -> Nick -> Result
initializeGame (NoGame configuration) serverName channelName userName lambdabotName = Result output updatedGame
 where
  updatedGame = newGame phrase updatedConfiguration serverName channelName userName lambdabotName
  (updatedConfiguration, phrase) = selectPhrase configuration
  begun = messageNewGameHasBegun configuration
  output = (:) begun $ showGame updatedGame
initializeGame previous _ _ _ _ = Result [gameInProgress] previous

showGame :: Game -> Messages
showGame (NoGame _) = [noGameInProgress]
showGame (InGame gameState configuration) = board : guesses
 where
  board = showBoard gameState configuration
  guesses = showGuesses gameState configuration

showBoard :: GameState -> Configuration -> Message
showBoard gameState@(GameState _ correct _ _ _ _ answer _ _ _ _) configuration = message
 where
  message = substituteTokens (messageToUse configuration) "@" [board]
  board = T.intersperse '.' boardState
  boardState = T.map (`transformLetter` correct) answer
  messageToUse = if isSolved gameState then messageGuessed else messageGuessing

showGuesses :: GameState -> Configuration -> Messages
showGuesses gameState configuration = [remainingGuesses, incorrectGuesses]
 where
  remaining = calculateRemaining gameState incorrect
  remainingGuesses = substituteTokens (messageNumberOfGuessesRemaining configuration) "@" [numberOfGuessesRemaining]
  incorrectGuesses = substituteTokens (messageIncorrectGuessesTried configuration) "@" [incorrect]
  numberOfGuessesRemaining = T.pack $ show remaining
  incorrect = incorrectLetters gameState

calculateRemaining :: GameState -> Phrase -> Int
calculateRemaining gameState incorrect = allowedMisses gameState - T.length incorrect

substituteTokens :: Phrase -> Phrase -> [Phrase] -> Phrase
substituteTokens template token values = message
 where
  segments = T.splitOn token template
  message = T.concat $ (+++) segments values

transformLetter :: Char -> Phrase -> Char
transformLetter ' ' _ = ' '
transformLetter letter correctGuesses
  | letter `elemText` correctGuesses = letter
  | otherwise = '_'

completeSetOfLetters :: GameState -> Phrase
completeSetOfLetters gameState = T.pack $ M.keys $ T.foldl hist M.empty (T.filter (/= ' ') (target gameState))

isSolved :: GameState -> Bool
isSolved gameState = T.length (correctLetters gameState) == M.size (T.foldl hist M.empty (T.filter (/= ' ') (target gameState)))

getMostPopularGuess :: GameState -> Maybe Char
getMostPopularGuess gameState
  | T.null letters = Nothing
  | otherwise = Just $ fst (M.foldlWithKey most (' ', 0 :: Int) (T.foldl hist M.empty letters))
 where
  letters = userLetters gameState

most :: Ord k => Ord a => Num a => (k, a) -> k -> a -> (k, a)
most (pk, pc) ck cc = if pc < cc then (ck, cc) else (pk, pc)

type Histogram k a = M.Map k a

hist :: Ord k => Num a => Histogram k a -> k -> Histogram k a
hist m c = M.insertWith (+) c 1 m

incorrectArgumentsForStart :: Message
incorrectArgumentsForStart = "Incorrect number of arguments, please do not add parameters to start command."

incorrectArgumentsForShow :: Message
incorrectArgumentsForShow = "Incorrect number of arguments, please do not add parameters to show command."

incorrectArgumentsForProgress :: Message
incorrectArgumentsForProgress = "Incorrect number of arguments, please do not add parameters to next command."

incorrectArgumentsForFinalAnswer :: Message
incorrectArgumentsForFinalAnswer = "Incorrect number of arguments, please include your guess for the whole phrase."

incorrectArgumentsForAppend :: Message
incorrectArgumentsForAppend = "Incorrect number of arguments, please include the character you want to guess."

incorrectArgumentsForAddPhrase :: Message
incorrectArgumentsForAddPhrase = "Incorrect number of arguments, please include the phrase you want to add."

incorrectArgumentsForRemovePhrase :: Message
incorrectArgumentsForRemovePhrase = "Incorrect number of arguments, please include the phrase you want to remove."

noGameInProgress :: Message
noGameInProgress = "No game is in progress, use ?hangman-start to start a new game."

gameInProgress :: Message
gameInProgress = "A game is in progress, please complete this game before starting a new one."

messageIncorrectArgumentsForConfigure :: Message
messageIncorrectArgumentsForConfigure = "Incorrect number of arguments, please use ?hangman-configure [option] [value]."

messageUnknownConfigurationOption :: Message
messageUnknownConfigurationOption = "Unknown configuration option"

messageConfigurationUpdated :: Message
messageConfigurationUpdated = "Configuration updated"

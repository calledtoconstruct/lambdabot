{-# LANGUAGE OverloadedStrings #-}

module Lambdabot.Plugin.Hangman.Hangman (hangmanPlugin) where

import Lambdabot.Config.Hangman (hangmanPhrases)
import Lambdabot.Plugin.Hangman.Background (maybeStartTimer, signalShutdown)

import qualified Lambdabot.Message as Msg (channels, lambdabotName, nick, server)
import Lambdabot.Plugin (
  Cmd,
  Module (..),
  MonadConfig (getConfig),
  Nick,
  aliases,
  command,
  help,
  newModule,
  privileged,
  process,
  say,
  stdSerial,
  withMS,
  withMsg, readMS
 )
import Lambdabot.Plugin.Hangman.Configuration (Phrase, newConfiguration)
import Lambdabot.Plugin.Hangman.Game (
  Game (..),
  GameState (..),
  Messages,
  Result (game, messages),
  incorrectArgumentsForAddPhrase,
  incorrectArgumentsForAppend,
  incorrectArgumentsForFinalAnswer,
  incorrectArgumentsForProgress,
  incorrectArgumentsForRemovePhrase,
  incorrectArgumentsForShow,
  incorrectArgumentsForStart,
  initializeGame,
  messageIncorrectArgumentsForConfigure,
  showGame, HangmanState, Hangman
 )
import Lambdabot.Plugin.Hangman.Logic (addGuess, guessFinal, progressGame)
import Lambdabot.Plugin.Hangman.Manage (addPhrase, configure, removePhrase)

import Control.Monad.Trans (lift)
import Data.List (partition)
import qualified Data.Text as T
import Lambdabot.Nick (fmtNick)

hangmanPlugin :: Module HangmanState
hangmanPlugin =
  newModule
    { moduleSerialize = Just stdSerial
    , moduleDefState = pure []
    , moduleInit = do
      games <- readMS
      maybeStartTimer games
    , moduleExit = signalShutdown
    , moduleCmds =
        return
          [ (command "hangman-start")
              { help = say "hangman-start - Starts the game."
              , process = commandStartGame . T.pack
              }
          , (command "hangman-status")
              { aliases = ["hangman-state"]
              , help = say "hangman-status - Prints the current state of the game."
              , process = commandStatus . T.pack
              }
          , (command "hangman-next")
              { help = say "hangman-next - Tallies the guesses and applies the most popular one."
              , process = commandNext . T.pack
              }
          , (command "hangman-final-answer")
              { help = say "hangman-final-answer - Guesses the phrase.  An incorrect guess decreases the number of wrong answers available."
              , process = commandFinalAnswer . T.pack
              }
          , (command "hangman-timer-tick")
              { help = say "hangman-timer-tick - For internal use only."
              , process = commandNext . T.pack
              }
          , (command "hangman-guess")
              { aliases = ["hg"]
              , help = say "hangman-guess [letter] - Provide your guess."
              , process = commandAppendGuess . T.pack
              }
          , (command "hangman-add")
              { help = say "hangman-add [phrase] - Add a new phrase to the database."
              , process = commandAddPhrase . T.pack
              , privileged = True
              }
          , (command "hangman-remove")
              { help = say "hangman-remove [phrase] - Remove a phrase from the database."
              , process = commandRemovePhrase . T.pack
              , privileged = True
              }
          , (command "hangman-configure")
              { help = say "hangman-configure [option] [value] - Update the configuration option to the value provided."
              , process = commandConfigure . T.pack
              , privileged = True
              }
          ]
    }

withCommandChannelGameState :: (Game -> String -> Nick -> Nick -> Nick -> (Game -> Cmd Hangman ()) -> Cmd Hangman ()) -> Cmd Hangman ()
withCommandChannelGameState f = withMS $ \games write -> withMsg $ \msg ->
  let thisServerName = Msg.server msg
      thisChannelName = head $ Msg.channels msg
      who = Msg.nick msg
      lbn = Msg.lambdabotName msg
      (thisChannel, otherChannels) = partition (gameFor thisServerName thisChannelName) games
   in do
        defaultPhrases <- getConfig hangmanPhrases
        let thisGame = if null thisChannel then NoGame (newConfiguration defaultPhrases) else head thisChannel
        f thisGame thisServerName thisChannelName who lbn $ \updatedGame -> do
          let updatedGames = updatedGame : otherChannels
          write updatedGames
          lift (maybeStartTimer updatedGames)

withChannelGameState :: (Game -> (Game -> Cmd Hangman ()) -> Cmd Hangman ()) -> Cmd Hangman ()
withChannelGameState f = withCommandChannelGameState $ \thisGame _ _ _ _ write -> f thisGame write

gameFor :: String -> Nick -> Game -> Bool
gameFor _ _ (NoGame _) = False
gameFor serverName channelName (InGame gameState _) = sameServer && sameChannel
 where
  sameServer = server gameState == serverName
  sameChannel = channel gameState == fmtNick serverName channelName

commandStartGame :: Phrase -> Cmd Hangman ()
commandStartGame phrase
  | T.null phrase = withCommandChannelGameState $ \thisGame thisServerName thisChannelName who lbn write -> do
    let result = initializeGame thisGame thisServerName thisChannelName who lbn
    sayMessages $ messages result
    write (game result)
  | otherwise = say $ T.unpack incorrectArgumentsForStart

commandStatus :: Phrase -> Cmd Hangman ()
commandStatus phrase
  | T.null phrase = withChannelGameState $ \thisGame _ -> sayMessages $ showGame thisGame
  | otherwise = say $ T.unpack incorrectArgumentsForShow

commandFinalAnswer :: Phrase -> Cmd Hangman ()
commandFinalAnswer phrase
  | T.null phrase = say $ T.unpack incorrectArgumentsForFinalAnswer
  | otherwise = withChannelGameState $ \thisGame write -> do
    let result = guessFinal thisGame phrase
    sayMessages (messages result)
    write (game result)

commandNext :: Phrase -> Cmd Hangman ()
commandNext phrase
  | T.null phrase = withChannelGameState $ \thisGame write -> do
    let result = progressGame thisGame
    sayMessages (messages result)
    write (game result)
  | otherwise = say $ T.unpack incorrectArgumentsForProgress

commandAppendGuess :: Phrase -> Cmd Hangman ()
commandAppendGuess phrase
  | T.null phrase = say $ T.unpack incorrectArgumentsForAppend
  | otherwise = withChannelGameState $ \thisGame write -> do
    let result = addGuess thisGame (T.head phrase)
    sayMessages (messages result)
    write (game result)

commandAddPhrase :: Phrase -> Cmd Hangman ()
commandAddPhrase phrase
  | T.null phrase = say $ T.unpack incorrectArgumentsForAddPhrase
  | otherwise = withChannelGameState $ \thisGame write -> do
    let result = addPhrase thisGame phrase
    sayMessages (messages result)
    write (game result)

commandRemovePhrase :: Phrase -> Cmd Hangman ()
commandRemovePhrase phrase
  | T.null phrase = say $ T.unpack incorrectArgumentsForRemovePhrase
  | otherwise = withChannelGameState $ \thisGame write -> do
    let result = removePhrase thisGame phrase
    sayMessages (messages result)
    write (game result)

commandConfigure :: Phrase -> Cmd Hangman ()
commandConfigure input
  | T.null input = say $ T.unpack messageIncorrectArgumentsForConfigure
  | otherwise = withChannelGameState $ \thisGame write -> do
    let result = configure thisGame input
    sayMessages (messages result)
    write (game result)

sayMessages :: Messages -> Cmd Hangman ()
sayMessages [] = return ()
sayMessages output = foldr1 (>>) $ say . T.unpack <$> output

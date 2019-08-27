{-# LANGUAGE DeriveGeneric #-}

module Lambdabot.Plugin.Hangman.Logic where

import Data.Universe.Helpers ((+++))
import Data.List (sortOn, sort, group, nub)
import Data.List.Split (splitOn)
import Data.Char (toUpper)
import System.Random (mkStdGen, random)
import GHC.Generics

-- TODO:
-- [ ]: Intercalate letters already used
-- [ ]: Incorporate emoji

data Game =
  NoGame Configuration
  | InGame GameState Configuration
  deriving (Generic, Show, Read)

data GameState = GameState {
  userLetters :: String,
  correctLetters :: String,
  incorrectLetters :: String,
  target :: String
}
  deriving (Generic, Show, Read)

data Configuration = Configuration {
  phrases :: [String],
  lastPhrase :: Int,
  allowedMisses :: Int,
  messagePhraseAdded :: String,
  messagePhraseRemoved :: String,
  messageYouWon :: String,
  messageYouLost :: String,
  messageThereWereNoGuesses :: String,
  messageNewGameHasBegun :: String,
  messageIncorrectGuessesTried :: String,
  messageNumberOfGuessesRemaining :: String,
  messageGuessing :: String
}
  deriving (Generic, Show, Read)

-- Current State:
-- Target: Twitch Love
-- User Letters: wcclaaabccccccc
-- Correct Letters: h
-- Incorrect Letters: z

-- Calculated:
-- Bad Guesses: z
-- Good Guesses: h
-- Popular Guess: c
-- Board: _ _ _ _ _ H   _ _ _ _
-- Man: Q________*
-- Man: Q_______*
-- Man: Q______*
-- Man: Q*
-- Man: KAA-POW!

incorrectArgumentsForStart :: String
incorrectArgumentsForStart = "Incorrect number of arguments, please do not add parameters to start command."

incorrectArgumentsForShow :: String
incorrectArgumentsForShow = "Incorrect number of arguments, please do not add parameters to show command."

incorrectArgumentsForProgress :: String
incorrectArgumentsForProgress = "Incorrect number of arguments, please do not add parameters to final answer command."

incorrectArgumentsForAppend :: String
incorrectArgumentsForAppend = "Incorrect number of arguments, please include the character you want to guess."

incorrectArgumentsForAddPhrase :: String
incorrectArgumentsForAddPhrase = "Incorrect number of arguments, please include the phrase you want to add."

incorrectArgumentsForRemovePhrase :: String
incorrectArgumentsForRemovePhrase = "Incorrect number of arguments, please include the phrase you want to remove."

noGameInProgress :: String
noGameInProgress = "No game is in progress, use ?hangman-start to start a new game."

gameInProgress :: String
gameInProgress = "A game is in progress, please complete this game before starting a new one."

initializeGame :: Game -> ([String], Game)
initializeGame (NoGame configuration) = (message: [], updatedState)
    where updatedState = newGame phrase updatedConfiguration
          (updatedConfiguration, phrase) = selectPhrase configuration
          message = messageNewGameHasBegun configuration
initializeGame game = ([gameInProgress], game)

newGame :: String -> Configuration -> Game
newGame answer = InGame (GameState {
  target = answer,
  userLetters = "",
  correctLetters = "",
  incorrectLetters = ""
})

newConfiguration :: Configuration
newConfiguration = Configuration {
  phrases = [
    "MONKATOS",
    "TWITCH SINGS",
    "BEST STREAMER",
    "IN REAL LIFE",
    "SCIENCE AND TECHNOLOGY",
    "SOFTWARE ENGINEERING",
    "HASKELL RULEZ"
  ],
  lastPhrase = 0,
  allowedMisses = 10,
  messagePhraseAdded = "Phrase added",  
  messagePhraseRemoved = "Phrase removed",  
  messageYouWon = "You win!",  
  messageYouLost = "You lost!",  
  messageThereWereNoGuesses = "There were no guesses!  Use ?hangman-guess [letter] to add a letter you believe is in the phrase.  The most popular guess will be evaluated.",  
  messageNewGameHasBegun = "A new game of Hangman has begun!  Guess the first letter using:  ?hangman-guess [letter]",  
  messageIncorrectGuessesTried = "The following guesses were incorrect or duplicate: [@]",  
  messageNumberOfGuessesRemaining = "You will lose if you make @ more mistake(s).",
  messageGuessing = "You are guessing this phrase: [@]"
}

getConfiguration :: Game -> Configuration
getConfiguration (NoGame configuration) = configuration
getConfiguration (InGame _ configuration) = configuration

selectPhrase :: Configuration -> (Configuration, String)
selectPhrase configuration = (updatedConfiguration, phrase)
  where lengthOfPhrases = length $ listOfPhrases
        generator = mkStdGen $ lastPhrase configuration
        (selected, _) = random generator
        phrase = listOfPhrases !! (selected `mod` lengthOfPhrases)
        updatedConfiguration = configuration { lastPhrase = selected }
        listOfPhrases = phrases configuration

addGuess :: Game -> Char -> ([String], Game)
addGuess game letter = ([], finalState)
  where finalState = modifyState game $ toUpper letter

modifyState :: Game -> Char -> Game
modifyState (NoGame configuration) _ = NoGame configuration
modifyState (InGame game configuration) char = InGame game { userLetters = char: userLetters game } configuration

progressGame :: Game -> ([String], Game)
progressGame (NoGame configuration) = ([noGameInProgress], NoGame configuration)
progressGame game@(InGame gameState configuration) =
  case getMostPopularGuess gameState of
    Just guess -> applyGuess gameState configuration guess
    Nothing -> ([messageThereWereNoGuesses configuration], game)

applyGuess :: GameState -> Configuration -> Char -> ([String], Game)
applyGuess gameState configuration popular = (messages, updatedGame)
  where correct = isCorrect gameState popular
        result = case correct of
          Just True -> "correct"
          Just False -> "incorrect"
          Nothing -> "already guessed"
        outcome = "The popular guess was " ++ popular: " and that was " ++ result ++ "."
        updatedGameState = updateGameState gameState popular correct
        (evaluation, maybeGameState) = evaluateGame updatedGameState configuration
        updatedGame = case maybeGameState of
          Nothing -> NoGame configuration
          Just finalGameState -> InGame finalGameState configuration
        messages = outcome: evaluation

evaluateGame :: GameState -> Configuration -> ([String], Maybe GameState)
evaluateGame gameState@(GameState _ correct incorrect answer) configuration
  | remaining <= 0 = ([board, messageYouLost configuration], Nothing)
  | length correct == length (nub $ filter (/= ' ') answer) = ([board, messageYouWon configuration], Nothing)
  | otherwise = (board: guesses, Just gameState)
  where remaining = calculateRemaining configuration incorrect
        guesses = showGuesses gameState configuration
        board = showBoard gameState configuration

showGuesses :: GameState -> Configuration -> [String]
showGuesses (GameState _ _ incorrect _) configuration = [remainingGuesses, incorrectGuesses]
  where remaining = calculateRemaining configuration incorrect
        remainingGuesses = substituteTokens (messageNumberOfGuessesRemaining configuration) "@" $ [numberOfGuessesRemaining]
        incorrectGuesses = substituteTokens (messageIncorrectGuessesTried configuration) "@" [incorrect]
        numberOfGuessesRemaining = show remaining

substituteTokens :: String -> String -> [String] -> String
substituteTokens template token values = message
  where segments = splitOn token template
        message = concat $ (+++) segments values

calculateRemaining :: Configuration -> String -> Int
calculateRemaining configuration incorrect = (allowedMisses configuration) - length incorrect

getMostPopularGuess :: GameState -> Maybe Char
getMostPopularGuess (GameState letters _ _ _)
  | null letters = Nothing
  | otherwise = Just $ last $ concat $ sortOn length $ group $ sort letters

isCorrect :: GameState -> Char -> Maybe Bool
isCorrect (GameState _ correct _ answer) letter = case foundInTarget of
  True -> case foundInCorrectLetters of
    True -> Nothing
    False -> Just True
  False -> Just False
  where foundInTarget = elem letter answer
        foundInCorrectLetters = elem letter correct

updateGameState :: GameState -> Char -> Maybe Bool -> GameState
updateGameState (GameState _ correct incorrect answer) letter (Just True) = GameState {
  userLetters = [],
  correctLetters = letter: correct,
  incorrectLetters = incorrect,
  target = answer
}
updateGameState (GameState _ correct incorrect answer) letter (Just False) = GameState {
  userLetters = [],
  correctLetters = correct,
  incorrectLetters = letter: incorrect,
  target = answer
}
updateGameState (GameState _ correct incorrect answer) letter Nothing = GameState {
  userLetters = [],
  correctLetters = correct,
  incorrectLetters = letter: incorrect,
  target = answer
}

showGame :: Game -> [String]
showGame (NoGame _) = [noGameInProgress]
showGame (InGame gameState configuration) = board: guesses
    where board = showBoard gameState configuration
          guesses = showGuesses gameState configuration

showBoard :: GameState -> Configuration -> String
showBoard (GameState _ correct _ answer) configuration = message
  where message = substituteTokens (messageGuessing configuration) "@" [board]
        board = intercalate '.' boardState
        boardState = map (transformLetter correct) answer

showInternalState :: Game -> String
showInternalState (NoGame _) = noGameInProgress
showInternalState (InGame (GameState user correct incorrect answer) _) = output
    where output = user ++ ":" ++ correct ++ ":" ++ incorrect ++ ":" ++ answer

transformLetter :: [Char] -> Char -> Char
transformLetter correct letter = case elem letter $ ' ': correct of
  True -> letter
  False -> '_'

intercalate :: Char -> [Char] -> [Char]
intercalate _ [] = []
intercalate value (first: rest)
  | null ending = first: ending
  | otherwise = first: value: ending
  where ending = intercalate value rest

addPhrase :: Game -> String -> Game
addPhrase (NoGame configuration) phrase = NoGame $ addPhrase' configuration phrase
addPhrase (InGame gameState configuration) phrase = InGame gameState $ addPhrase' configuration phrase

addPhrase' :: Configuration -> String -> Configuration
addPhrase' configuration phrase = configuration { phrases = upperPhrase: (phrases configuration) }
  where upperPhrase = map toUpper phrase

removePhrase :: Game -> String -> Game
removePhrase (NoGame configuration) phrase = NoGame $ removePhrase' configuration phrase
removePhrase (InGame gameState configuration) phrase = InGame gameState $ removePhrase' configuration phrase

removePhrase' :: Configuration -> String -> Configuration
removePhrase' configuration phrase = configuration { phrases = filtered }
  where filtered = filter (/= upperPhrase) $ phrases configuration
        upperPhrase = map toUpper phrase

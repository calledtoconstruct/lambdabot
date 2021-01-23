{-# LANGUAGE PatternGuards #-}

{- |   The Type Module - another progressive plugin for lambdabot

 pesco hamburg 2003-04-05

     Greetings reader,

     whether you're a regular follower of the series or dropping in for
     the first time, let me present for your pleasure the Type Module:

     One thing we enjoy on #haskell is throwing function types at each
     other instead of spelling out tiresome monologue about arguments
     or return values. Unfortunately such a toss often involves a local
     lookup of the type signature in question because one is seldom
     sure about the actual argument order.

     Well, what do you know, this plugin enables lambdabot to automate
     that lookup for you and your fellow lambda hackers.
-}
module Lambdabot.Plugin.Haskell.Type (typePlugin, queryGHCI) where

import Codec.Binary.UTF8.String (decodeString, encodeString)
import Control.Monad ((<=<))
import Data.Char (isSpace)
import Data.Maybe (mapMaybe)
import Lambdabot.Config.Haskell (ghciBinary, languageExts)
import Lambdabot.Plugin (
  Cmd,
  Command (help, process),
  Module (contextual, moduleCmds),
  MonadConfig (getConfig),
  MonadLB,
  command,
  newModule,
  say,
 )
import Lambdabot.Plugin.Haskell.Eval (findL_hs)
import Lambdabot.Util (expandTab, io)
import System.Process (readProcessWithExitCode)
import Text.Regex.TDFA (MatchResult (mrAfter, mrSubList), Regex, RegexContext (matchM), RegexMaker (makeRegex), (=~), (=~~))

typePlugin :: Module ()
typePlugin =
  newModule
    { moduleCmds =
        return
          [ (command "type")
              { help = say "type <expr>. Return the type of a value"
              , process = runit ":t"
              }
          , (command "kind")
              { help = say "kind <type>. Return the kind of a type"
              , process = runit ":k"
              }
          ]
    , contextual = \text ->
        let (prefix, expr) = splitAt 3 text
         in case prefix of
              ":t " -> runit ":t" expr
              ":k " -> runit ":k" expr
              _ -> return ()
    }

runit :: MonadLB m => String -> String -> Cmd m ()
runit s expr = queryGHCI s expr >>= say

--     In accordance with the KISS principle, the plan is to delegate all
--     the hard work! To get the type of foo, pipe

theCommand :: [Char] -> [Char] -> [Char]
theCommand cmd foo = cmd ++ " " ++ foo

--     into GHCi and send any line matching

signatureRegex :: Regex
signatureRegex = makeRegex "^(\\*?[A-Z][_a-zA-Z0-9]*(\\*?[A-Z][_a-zA-Z0-9]*)*>)? *(.*[       -=:].*)"

--
-- Rather than use subRegex, which is new to 6.4, we can remove comments
-- old skool style.
-- Former regex for this:
--    "(\\{-[^-]*-+([^\\}-][^-]*-+)*\\}|--.*$)"
--
stripComments :: String -> String
stripComments [] = []
stripComments ('\n' : _) = [] -- drop any newwline and rest. *security*
stripComments ('-' : '-' : _) = [] --
stripComments ('{' : '-' : cs) = stripComments (go 1 cs)
stripComments (c : cs) = c : stripComments cs

-- Adapted from ghc/compiler/parser/Lexer.x
go :: Int -> String -> String
go 0 xs = xs
go _ ['-'] = [] -- unterminated
go n ('-' : x : xs)
  | x == '}' = go (n -1) xs
  | otherwise = go n (x : xs)
go _ ['{'] = [] -- unterminated
go n ('{' : x : xs)
  | x == '-' = go (n + 1) xs
  | otherwise = go n (x : xs)
go n (_ : xs) = go n xs
go _ _ = [] -- unterminated

--     through IRC.

--
--     We filtering out the lines that match our regex,
--     selecting the last subset match on each matching line before finally concatting
--     the whole lot together again.
--
extractSignatures :: String -> Maybe String
extractSignatures =
  fmap reverse . removeExp . reverse . (' ' :) . unwords
    . map (dropWhile isSpace . expandTab 8)
    . mapMaybe ((last' . mrSubList) <=< matchM signatureRegex)
    . lines
 where
  last' [] = Nothing
  last' xs = Just $ last xs
  removeExp [] = Nothing
  removeExp xs = removeExp' 0 xs
  removeExp' 0 (' ' : ':' : ':' : ' ' : _) = Just []
  removeExp' n ('(' : xs) = ('(' :) `fmap` removeExp' (n + 1) xs
  removeExp' n (')' : xs) = (')' :) `fmap` removeExp' (n -1) xs
  removeExp' n (x : xs) = (x :) `fmap` removeExp' n xs
  removeExp' _ [] = Nothing

  removeExp :: String -> Maybe String

  removeExp' :: Int -> String -> Maybe String

--
--     With this the command handler can be easily defined using readProcessWithExitCode:
--
queryGHCI :: MonadLB m => String -> String -> m String
queryGHCI cmd expr = do
  l <- findL_hs
  exts <- getConfig languageExts
  let context = ":load " ++ l ++ "\n:m *L\n" -- using -fforce-recomp to make sure we get *L in scope instead of just L
      extFlags = ["-X" ++ ext | ext <- exts]
  ghci <- getConfig ghciBinary
  (_, output, errors) <-
    io $
      readProcessWithExitCode
        ghci
        ("-v0" : "-fforce-recomp" : "-iState" : "-ignore-dot-ghci" : extFlags)
        (context ++ theCommand cmd (stripComments (decodeString expr)))
  let ls = extractSignatures output
  return $ case ls of
    Nothing ->
      encodeString . unlines . take 3 . filter (not . null) . map cleanRE2
        . lines
        . expandTab 8
        . cleanRE
        . filter (/= '\r')
        $ errors -- "bzzt"
    Just t -> t
 where
  cleanRE, cleanRE2 :: String -> String
  cleanRE s
    | s =~ notfound = "Couldn\'t find qualified module."
    | Just m <- s =~~ ghci_msg = mrAfter m
    | otherwise = s
  cleanRE2 s
    | Just m <- s =~~ ghci_msg = mrAfter m
    | otherwise = s
  ghci_msg = "<interactive>:[^:]*:[^:]*: ?"
  notfound = "Failed to load interface"

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternGuards #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns -Wmissing-signatures #-}

-- (c) 2005 Samuel Bronson

module Lambdabot.Plugin.Misc.Todo (todoPlugin) where

import Lambdabot.Compat.PackedNick (packNick, unpackNick)
import Lambdabot.Plugin (
  Cmd,
  LB,
  Module,
  ModuleT,
  Packable (
    readPacked,
    showPacked
  ),
  Serial (..),
  aliases,
  command,
  getSender,
  help,
  modifyMS,
  moduleCmds,
  moduleDefState,
  moduleSerialize,
  newModule,
  privileged,
  process,
  readM,
  readMS,
  say,
  showNick,
  withMS,
 )
import Lambdabot.Util (readPackedEntry)

import Codec.Compression.GZip (compress, decompress)
import Control.Monad (forM_, zipWithM)
import qualified Data.ByteString.Char8 as P (ByteString, concat, lines, pack, unlines, unpack)
import Data.ByteString.Lazy (fromChunks, toChunks)

gzip :: P.ByteString -> P.ByteString
gzip = P.concat . toChunks . compress . fromChunks . (: [])

gunzip :: P.ByteString -> P.ByteString
gunzip = P.concat . toChunks . decompress . fromChunks . (: [])

data CompletionState = Complete | Incomplete | Deferred deriving (Eq, Show, Read)
type AssociativeCompletionState = (P.ByteString, P.ByteString, CompletionState)

instance Packable TodoState where
  readPacked ps = readPackedEntry (splitAt 3) buildAssociativeCompletionState (P.lines . gunzip $ ps)
  showPacked = gzip . P.unlines . concatMap (\(k, v, s) -> [k, v, P.pack . show $ s])

buildAssociativeCompletionState :: [P.ByteString] -> AssociativeCompletionState
buildAssociativeCompletionState (k : v : s : _) = (k, v, read . P.unpack $ s)

completionStateAssociativeListPackedSerial :: Serial [AssociativeCompletionState]
completionStateAssociativeListPackedSerial = Serial (Just . showPacked) (Just . readPacked)

type TodoState = [AssociativeCompletionState]
type Todo = ModuleT TodoState LB
type IndexedEntry = (Int, AssociativeCompletionState)
type TodoStateTransform = ([IndexedEntry] -> [IndexedEntry])
type TodoStateTransformResult = (Maybe TodoState, String)

todoPlugin :: Module TodoState
todoPlugin =
  newModule
    { moduleDefState = return ([] :: TodoState)
    , moduleSerialize = Just completionStateAssociativeListPackedSerial
    , moduleCmds =
        return
          [ (command "todo")
              { help = say "todo. List todo entries"
              , process = getTodo
              , aliases = ["list-todo", "list-todos"]
              }
          , (command "add-todo")
              { help = say "add-todo <idea>. Add a todo entry"
              , process = addTodo
              , aliases = ["todo-add"]
              }
          , (command "update-todo")
              { help = say "update-todo <index> <complete>. Mark a todo entry as complete (True) or incomplete (False)"
              , process = updateTodo
              , aliases = ["mark-todo"]
              }
          , (command "defer-todo")
              { help = say "defer-todo <index> <deferred>. Mark a todo entry as deferred (True) or incomplete (False)"
              , process = deferTodo
              }
          , (command "delete-todo")
              { privileged = True
              , help = say "todo-delete <index>. Delete a todo entry (for admins)"
              , process = delTodo
              , aliases = ["todo-delete", "remove-todo", "todo-remove"]
              }
          ]
    }

getTodo :: String -> Cmd Todo ()
getTodo [] = readMS >>= sayTodo
getTodo _ = say "?todo has no args, try ?add-todo or ?list todo"

sayTodo :: TodoState -> Cmd Todo ()
sayTodo [] = say "Nothing to do!"
sayTodo todoList =
  mapM_ say =<< zipWithM fmtTodoItem ([1 ..] :: [Int]) todoList
 where
  fmtTodoItem n (idea, nick_, state) = do
    nick <- showNick (unpackNick nick_)
    return $ concat [show n, ". ", nick, ": [", checkMark, "] ", P.unpack idea]
   where
    checkMark = case state of
      Complete -> "X"
      Incomplete -> " "
      Deferred -> "?"

addTodo :: String -> Cmd Todo ()
addTodo rest = do
  sender <- fmap packNick getSender
  modifyMS (++ [(P.pack rest, sender, Incomplete)])
  say "Entry added to the todo list"

toggleDeferred :: CompletionState -> CompletionState
toggleDeferred state
  | state == Deferred = Incomplete
  | otherwise = Deferred

deferTodo :: String -> Cmd Todo ()
deferTodo rest = updateState rest (updateCompletionStateByIndex toggleDeferred) "Syntax error.  ?defer-todo <index>, where index :: Int"

toggleComplete :: CompletionState -> CompletionState
toggleComplete state
  | state == Incomplete = Complete
  | otherwise = Complete

updateTodo :: String -> Cmd Todo ()
updateTodo rest = updateState rest (updateCompletionStateByIndex toggleComplete) "Syntax error. ?todo <index>, where index :: Int"

delTodo :: String -> Cmd Todo ()
delTodo rest = updateState rest filterByIndex "Syntax error. ?todo <n>, where n :: Int"

filterByIndex :: Int -> [IndexedEntry] -> [IndexedEntry]
filterByIndex index = filter ((/= index) . fst)

updateCompletionStateByIndex :: (CompletionState -> CompletionState) -> Int -> [IndexedEntry] -> [IndexedEntry]
updateCompletionStateByIndex getNewState index = map $ copyOrReplace getNewState index

copyOrReplace :: (CompletionState -> CompletionState) -> Int -> IndexedEntry -> IndexedEntry
copyOrReplace getNewState index (itemNumber, (who, what, state))
  | index == itemNumber = (itemNumber, (who, what, getNewState state))
  | otherwise = (itemNumber, (who, what, state))

updateState :: String -> (Int -> TodoStateTransform) -> String -> Cmd Todo ()
updateState rest transform errorMessage
  | Just n <- readM rest =
    say
      =<< withMS
        ( \list write -> do
            let (updated, message) = updateItem' (transform n) n list
            forM_ updated write
            return message
        )
  | otherwise = say errorMessage

updateItem' :: TodoStateTransform -> Int -> TodoState -> TodoStateTransformResult
updateItem' operation index list
  | null list = (Nothing, "Todo list is empty")
  | index > length list || index < 1 = (Nothing, show index ++ " is out of range")
  | otherwise = do
    let updated = map snd . operation . zip [1 ..] $ list
    let (a, _, _) = list !! (index - 1)
    (Just updated, "Updated: " ++ P.unpack a)

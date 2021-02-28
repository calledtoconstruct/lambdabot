{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternGuards #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns -Wmissing-signatures #-}

-- (c) 2005 Samuel Bronson

module Lambdabot.Plugin.Misc.Todo (todoPlugin) where

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
  getTarget,
  help,
  moduleCmds,
  moduleDefState,
  moduleSerialize,
  nName,
  newModule,
  privileged,
  process,
  randomSuccessMsg,
  readM,
  readMS,
  say,
  withMS,
 )

import Codec.Compression.GZip (compress, decompress)
import Control.Monad (when, zipWithM)
import qualified Data.ByteString.Char8 as P (ByteString, concat, lines, pack, unlines, unpack, empty)
import Data.ByteString.Lazy (fromChunks, toChunks)
import Data.List (partition)
import Data.Maybe (fromJust, isJust)

gzip :: P.ByteString -> P.ByteString
gzip = P.concat . toChunks . compress . fromChunks . (: [])

gunzip :: P.ByteString -> P.ByteString
gunzip = P.concat . toChunks . decompress . fromChunks . (: [])

type ChannelName = P.ByteString
data CompletionState = Complete | Incomplete | Deferred deriving (Eq, Show, Read)
type Todo = (P.ByteString, P.ByteString, CompletionState)

instance Packable TodoState where
  readPacked ps = consumePackedChannel [] (P.lines . gunzip $ ps)
  showPacked = gzip . P.unlines . concatMap producePackedChannel

producePackedChannel :: (ChannelName, [Todo]) -> [P.ByteString]
producePackedChannel (thisChannelName, todos) = thisChannelName : concatMap producePackedTodo todos ++ [P.empty]

producePackedTodo :: Todo -> [P.ByteString]
producePackedTodo (todo, sender, todoState) = [todo, sender, P.pack . show $ todoState]

consumePackedChannel :: [(ChannelName, [Todo])] -> [P.ByteString] -> [(ChannelName, [Todo])]
consumePackedChannel existing [] = existing
consumePackedChannel existing (next : rest) =
  let (todos, rest') = consumePackedTodo [] rest
   in consumePackedChannel ((next, todos) : existing) rest'

consumePackedTodo :: [Todo] -> [P.ByteString] -> ([Todo], [P.ByteString])
consumePackedTodo existing [_] = (existing, [])
consumePackedTodo existing ~(a : b : c : rest) = (buildTodo [a, b, c] : existing, rest)

buildTodo :: [P.ByteString] -> Todo
buildTodo ~(k : v : s : _) = (k, v, read . P.unpack $ s)

serializeTodoState :: Serial TodoState
serializeTodoState = Serial (Just . showPacked) (Just . readPacked)

type TodoState = [(ChannelName, [Todo])]
type TodoList = ModuleT TodoState LB
type IndexedEntry = (Int, Todo)
type TodoStateTransform = ([IndexedEntry] -> [IndexedEntry])
type TodoStateTransformResult = (Maybe [Todo], String)

todoPlugin :: Module TodoState
todoPlugin =
  newModule
    { moduleDefState = return ([] :: TodoState)
    , moduleSerialize = Just serializeTodoState
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

getTodo :: String -> Cmd TodoList ()
getTodo [] = readMS >>= sayTodo
getTodo _ = say "?todo has no args, try ?add-todo or ?list todo"

sayTodo :: TodoState -> Cmd TodoList ()
sayTodo [] = say "Nothing to do!"
sayTodo todoList = do
  thisChannelName <- P.pack . nName <$> getTarget
  let thisChannelState = filter ((== thisChannelName) . fst) todoList
  if null thisChannelState
    then say "Nothing to do!"
    else do
      let thisChannel = head thisChannelState
      mapM_ say =<< zipWithM fmtTodoItem ([1 ..] :: [Int]) (snd thisChannel)
 where
  fmtTodoItem n (idea, nick, state) = do
    return $ concat [show n, ". ", P.unpack nick, ": [", checkMark, "] ", P.unpack idea]
   where
    checkMark = case state of
      Complete -> "X"
      Incomplete -> " "
      Deferred -> "?"

addTodo :: String -> Cmd TodoList ()
addTodo rest = withMS $ \todoState write -> do
  sender <- P.pack . nName <$> getSender
  thisChannelName <- P.pack . nName <$> getTarget
  let (thisChannelState, otherChannels) = partition ((== thisChannelName) . fst) todoState
  let thisChannel = if not (null thisChannelState) then head thisChannelState else (thisChannelName, [])
  if any ((== P.pack rest) . getTodoMessage) $ snd thisChannel
    then say "That item already exists."
    else let updated = (P.pack rest, sender, Incomplete) : snd thisChannel
     in do
       write $ (thisChannelName, updated) : otherChannels
       say =<< randomSuccessMsg
    where getTodoMessage :: (a, b, c) -> a
          getTodoMessage (a, _, _) = a

toggleDeferred :: CompletionState -> CompletionState
toggleDeferred state
  | state == Deferred = Incomplete
  | otherwise = Deferred

deferTodo :: String -> Cmd TodoList ()
deferTodo rest = updateState rest (updateCompletionStateByIndex toggleDeferred) "Syntax error.  ?defer-todo <index>, where index :: Int"

toggleComplete :: CompletionState -> CompletionState
toggleComplete state
  | state == Incomplete = Complete
  | otherwise = Complete

updateTodo :: String -> Cmd TodoList ()
updateTodo rest = updateState rest (updateCompletionStateByIndex toggleComplete) "Syntax error. ?todo <index>, where index :: Int"

delTodo :: String -> Cmd TodoList ()
delTodo rest = updateState rest filterByIndex "Syntax error. ?todo <n>, where n :: Int"

filterByIndex :: Int -> [IndexedEntry] -> [IndexedEntry]
filterByIndex index = filter ((/= index) . fst)

updateCompletionStateByIndex :: (CompletionState -> CompletionState) -> Int -> [IndexedEntry] -> [IndexedEntry]
updateCompletionStateByIndex getNewState index = map $ copyOrReplace getNewState index

copyOrReplace :: (CompletionState -> CompletionState) -> Int -> IndexedEntry -> IndexedEntry
copyOrReplace getNewState index (itemNumber, (who, what, state))
  | index == itemNumber = (itemNumber, (who, what, getNewState state))
  | otherwise = (itemNumber, (who, what, state))

updateState :: String -> (Int -> TodoStateTransform) -> String -> Cmd TodoList ()
updateState rest transform errorMessage
  | Just n <- readM rest = withMS $ \todoState write -> do
    thisChannelName <- P.pack . nName <$> getTarget
    let (thisChannelState, otherChannels) = partition ((== thisChannelName) . fst) todoState
    let thisChannel = if not (null thisChannelState) then head thisChannelState else (thisChannelName, [])
    let (updated, message) = updateItem' (transform n) n $ snd thisChannel
    when (isJust updated) $ write $ (thisChannelName, fromJust updated) : otherChannels
    say message
  | otherwise = say errorMessage

updateItem' :: TodoStateTransform -> Int -> [Todo] -> TodoStateTransformResult
updateItem' operation index list
  | null list = (Nothing, "Todo list is empty")
  | index > length list || index < 1 = (Nothing, show index ++ " is out of range")
  | otherwise = do
    let updated = map snd . operation . zip [1 ..] $ list
    let (a, _, _) = list !! (index - 1)
    (Just updated, "Updated: " ++ P.unpack a)

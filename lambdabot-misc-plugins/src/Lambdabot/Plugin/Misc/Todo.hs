
{-# OPTIONS_GHC -fno-warn-overlapping-patterns -Wmissing-signatures #-}
{-# LANGUAGE PatternGuards #-}

-- (c) 2005 Samuel Bronson

module Lambdabot.Plugin.Misc.Todo (
  todoPlugin
) where

import Lambdabot.Compat.PackedNick
import Lambdabot.Plugin
import Control.Monad
import Data.Maybe
import qualified Data.ByteString.Char8 as P

type TodoState = [AssociativeCompletionState]
type Todo = ModuleT TodoState LB
type IndexedEntry = (Int, AssociativeCompletionState)
type TodoStateTransform = ([IndexedEntry] -> [IndexedEntry])
type TodoStateTransformResult = (Maybe TodoState, String)

todoPlugin :: Module TodoState
todoPlugin = newModule {
  moduleDefState  = return ([] :: TodoState),
  moduleSerialize = Just completionStateAssociativeListPackedSerial,
  moduleCmds = return [
    (command "todo") {
      help = say "todo. List todo entries",
      process = getTodo,
      aliases = ["list-todo", "list-todos"]
    },
    (command "add-todo") {
      help = say "add-todo <idea>. Add a todo entry",
      process = addTodo,
      aliases = ["todo-add"]
    },
    (command "update-todo") {
      help = say "update-todo <index> <complete>. Mark a todo entry as complete (True) or incomplete (False)",
      process = updateTodo,
      aliases = ["mark-todo"]
    },
    (command "delete-todo") {
      privileged = True,
      help = say "todo-delete <index>. Delete a todo entry (for admins)",
      process = delTodo,
      aliases = ["todo-delete", "remove-todo", "todo-remove"]
    }
  ]
}

getTodo :: String -> Cmd Todo ()
getTodo [] = readMS >>= sayTodo
getTodo _  = say "?todo has no args, try ?add-todo or ?list todo"

sayTodo :: TodoState -> Cmd Todo ()
sayTodo [] = say "Nothing to do!"
sayTodo todoList = do
  (mapM_ say) =<< zipWithM fmtTodoItem ([1..] :: [Int]) todoList
  where fmtTodoItem n (idea, nick_, state) = do
          nick <- showNick (unpackNick nick_)
          return $ concat $ [ show n, ". ", nick, ": [", if state then "X" else " ", "] ", P.unpack idea ]

addTodo :: String -> Cmd Todo ()
addTodo rest = do
  sender <- fmap packNick getSender
  modifyMS ( ++ [ (P.pack rest, sender, False) ] )
  say "Entry added to the todo list"

updateTodo :: String -> Cmd Todo ()
updateTodo rest = updateState rest updateCompletionStateByIndex "Syntax error. ?todo <index>, where index :: Int"

delTodo :: String -> Cmd Todo ()
delTodo rest = updateState rest filterByIndex "Syntax error. ?todo <n>, where n :: Int"

filterByIndex :: Int -> [IndexedEntry] -> [IndexedEntry]
filterByIndex index = filter ((/= index) . fst)

updateCompletionStateByIndex :: Int -> [IndexedEntry] -> [IndexedEntry]
updateCompletionStateByIndex index = map $ copyOrReplace index

copyOrReplace :: Int -> IndexedEntry -> IndexedEntry
copyOrReplace index (itemNumber, (who, what, state))
  | index == itemNumber = (itemNumber, (who, what, not state))
  | otherwise = (itemNumber, (who, what, state))

updateState :: String -> (Int -> TodoStateTransform) -> String -> Cmd Todo ()
updateState rest transform errorMessage
  | Just n <- readM rest = say =<< withMS (\list write -> do
    let (updated, message) = updateItem' (transform n) n list
    when (isJust updated) $ write $ fromJust updated
    return message)
  | otherwise = say errorMessage

updateItem' :: TodoStateTransform -> Int -> TodoState -> TodoStateTransformResult
updateItem' operation index list
  | null list                         = (Nothing, "Todo list is empty")
  | index > length list || index < 1  = (Nothing, (show index ++ " is out of range"))
  | otherwise                         = do
    let updated = (map snd . operation . zip [1..] $ list)
    let (a, _, _) = list !! (index - 1)
    (Just updated, ("Updated: " ++ P.unpack a))

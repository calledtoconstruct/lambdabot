
{-# LANGUAGE PatternGuards #-}

-- | A todo list
--
-- (c) 2005 Samuel Bronson

module Lambdabot.Plugin.Misc.Todo (
  todoPlugin
) where

import Lambdabot.Compat.PackedNick
import Lambdabot.Plugin
import Control.Monad
import qualified Data.ByteString.Char8 as P

-- A list of key/elem pairs with an ordering determined by its position in the list
type TodoState = [ AssociativeCompletionState ]
type Todo = ModuleT TodoState LB

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
    -- (command "update-todo") {
    --   help = say "update-todo <complete>. Mark a todo entry as complete (True) or incomplete (False)",
    --   process = updateTodo,
    --   aliases = ["mark-todo"]
    -- },
    (command "delete-todo") {
      privileged = True,
      help = say "todo-delete <n>. Delete a todo entry (for admins)",
      process = delTodo,
      aliases = ["todo-delete", "remove-todo", "todo-remove"]
    }
  ]
}

-- | Print todo list
getTodo :: String -> Cmd Todo ()
getTodo [] = readMS >>= sayTodo
getTodo _  = say "?todo has no args, try ?add-todo or ?list todo"

-- | Pretty print todo list
sayTodo :: [AssociativeCompletionState] -> Cmd Todo ()
sayTodo [] = say "Nothing to do!"
sayTodo todoList = say . unlines =<< zipWithM fmtTodoItem ([0..] :: [Int]) todoList
  where fmtTodoItem n (idea, nick_, state) = do
          nick <- showNick (unpackNick nick_)
          return $ concat $ [ show n, ". ", nick, ": ", P.unpack idea, if state then " is complete." else " is not complete." ]

-- | Add new entry to list
addTodo :: String -> Cmd Todo ()
addTodo rest = do
  sender <- fmap packNick getSender
  modifyMS ( ++ [ (P.pack rest, sender, False) ] )
  say "Entry added to the todo list"

-- | Delete an entry from the list
delTodo :: String -> Cmd Todo ()
delTodo rest
  | Just n <- readM rest = say =<< withMS (\ls write -> case () of
    _ | null ls                     -> return "Todo list is empty"
      | n > length ls - 1 || n < 0  -> return (show n ++ " is out of range") 
      | otherwise -> do
        write (map snd . filter ((/= n) . fst) . zip [0..] $ ls)
        let (a, _, _) = ls !! n
        return ("Removed: " ++ P.unpack a))
  | otherwise = say "Syntax error. ?todo <n>, where n :: Int"

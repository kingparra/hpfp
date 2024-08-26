#!/usr/bin/env stack
{- stack --resolver lts-20.18 script
   --package QuickCheck
   --package hspec
   --package checkers
   --package text
   --package optparse-applicative
-}

{-# LANGUAGE OverloadedStrings #-}

module TodoApp where

import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Options.Applicative
import System.Directory (getHomeDirectory)
import System.IO (hFlush, stdout)


data Task = Task {taskName :: T.Text} deriving (Show, Read)


data Command = Add String | List | Remove Int deriving (Show)


listTodos :: IO [Task]
listTodos = do
  homeDir <- getHomeDirectory
  let filePath = homeDir ++ "/todo.txt"
  tasks <- TIO.readFile filePath
  return $ map Task (T.lines tasks)

main :: IO ()
main = do
  command <- execParser opts
  case command of
    Add task -> addTask task
    List -> viewTasks
    Remove idx -> removeTask idx

opts :: ParserInfo Command
opts =
  info
    (commands <**> helper)
    ( fullDesc
        <> progDesc "Manage your todo list"
        <> header "todo-app - a simple command-line todo application"
    )

commands :: Parser Command
commands =
  subparser
    ( command "add" (info addTaskParser (progDesc "Add a new task"))
        <> command "list" (info listTaskParser (progDesc "List all tasks"))
        <> command "rm" (info removeTaskParser (progDesc "Remove a task"))
    )

addTaskParser :: Parser Command
addTaskParser =
  Add
    <$> strArgument
      ( metavar "TASK"
          <> help "The task to add"
      )

listTaskParser :: Parser Command
listTaskParser = pure List

removeTaskParser :: Parser Command
removeTaskParser =
  Remove
    <$> argument
      auto
      ( metavar "INDEX"
          <> help "The index of the task to remove"
      )

addTask :: String -> IO ()
addTask taskName = do
  homeDir <- getHomeDirectory
  let filePath = homeDir ++ "/todo.txt"
  let taskText = T.pack taskName
  tasks <- TIO.readFile filePath
  let taskCount = length (T.lines tasks) + 1
  TIO.appendFile filePath (taskText `T.append` "\n")
  putStrLn $ "Saved \"" ++ taskName ++ "\" to default todo list with index " ++ show taskCount

viewTasks :: IO ()
viewTasks = do
  homeDir <- getHomeDirectory
  let filePath = homeDir ++ "/todo.txt"
  tasks <- TIO.readFile filePath
  putStrLn "Your tasks:"
  mapM_ (putStrLn . formatTask) (zip [1 ..] (T.lines tasks))
  where
    formatTask (idx, task) = show idx ++ " " ++ T.unpack task

removeTask :: Int -> IO ()
removeTask idx = do
  homeDir <- getHomeDirectory
  let filePath = homeDir ++ "/todo.txt"
  tasks <- TIO.readFile filePath
  let taskLines = T.lines tasks
  let (before, after) = splitAt (idx - 1) taskLines
  let remainingTasks = before ++ drop 1 after
  TIO.writeFile filePath (T.unlines remainingTasks)
  case drop (idx - 1) taskLines of
    (removedTask : _) -> putStrLn $ "Removed task " ++ show idx ++ ", \"" ++ T.unpack removedTask ++ "\"."
    _ -> putStrLn "Invalid task index"

{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Main
-- Description : Lambda Calculus Interpreter Entry Point
--
-- This is the main entry point for the lambda interpreter.
-- It starts the REPL and handles command-line arguments.
--
-- Usage:
--   lambda-interpreter-exe          # Start REPL
--   lambda-interpreter-exe file.hs  # Execute file

module Main where

import REPL
import System.Environment (getArgs)
import System.Exit (exitSuccess, exitFailure)
import Data.List (isPrefixOf)

-- ============================================
-- Main Entry Point
-- ============================================

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      -- 启动 REPL
      putStrLn $ unlines
        [ "Lambda Calculus Interpreter"
        , "Type :help for help, :quit to exit"
        , ""
        ]
      result <- runREPL repl
      case result of
        Left "Goodbye!" -> exitSuccess
        Left err -> do
          putStrLn $ "Error: " ++ err
          exitFailure
        Right _ -> exitSuccess

    ["--help", "-h"] -> do
      printHelp
      exitSuccess

    ["--version", "-v"] -> do
      printVersion
      exitSuccess

    [file] -> do
      -- 执行文件
      result <- runREPL $ loadFile file >> repl
      case result of
        Left "Goodbye!" -> exitSuccess
        Left err -> do
          putStrLn $ "Error: " ++ err
          exitFailure
        Right _ -> exitSuccess

    _ -> do
      putStrLn "Invalid arguments. Use --help for usage."
      exitFailure

-- ============================================
-- 辅助函数
-- ============================================

printHelp :: IO ()
printHelp = putStrLn $ unlines
  [ "Lambda Calculus Interpreter"
  , ""
  , "Usage:"
  , "  lambda-interpreter-exe          Start REPL"
  , "  lambda-interpreter-exe <file>   Execute file"
  , ""
  , "Options:"
  , "  --help, -h     Show this help"
  , "  --version, -v  Show version"
  , ""
  , "REPL Commands:"
  , "  <expr>         Evaluate expression"
  , "  :t <expr>      Show type"
  , "  let x = expr   Define variable"
  , "  :load file     Load file"
  , "  :history       Show history"
  , "  :quit          Exit"
  , "  :help          Show help"
  , ""
  , "Examples:"
  , "  \\x -> x"
  , "  (\\x -> x + 1) 5"
  , "  let id = \\x -> x"
  , "  id 42"
  , ""
  ]

printVersion :: IO ()
printVersion = putStrLn $ unlines
  [ "Lambda Calculus Interpreter v0.1.0"
  , "Built with Haskell"
  , ""
  ]

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : REPL
-- Description : Read-Eval-Print Loop
--
-- This module implements an interactive REPL for the lambda interpreter.
--
-- Learning objectives:
--   * Understand Monad Transformers
--   * Learn to combine State, Except, and IO
--   * Build interactive applications

module REPL
  ( -- * REPL State
    REPLState(..)
  , initialState
    -- * REPL Monad
  , REPL
  , runREPL
    -- * Main Loop
  , repl
    -- * Commands
  , Command(..)
  , executeCommand
    -- * Helpers
  , loadFile
  , showHelp
  ) where

import AST
import TypeChecker
import Evaluator
import Parser
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.IO.Class
import System.IO
import Data.Maybe (isJust, fromJust)
import Data.List (isPrefixOf, intercalate)

-- ============================================
-- TODO 1: 定义 REPL 状态
-- ============================================

-- | REPL 状态
data REPLState = REPLState
  { typeEnv  :: TypeChecker.TypeEnv    -- ^ 类型环境
  , valueEnv :: Map String Expr        -- ^ 值环境
  , history  :: [String]               -- ^ 命令历史
  , counter  :: Int                    -- ^ 命令计数器
  } deriving (Show)

-- | 初始状态
initialState :: REPLState
initialState = REPLState
  { typeEnv = TypeChecker.emptyEnv
  , valueEnv = Map.empty
  , history = []
  , counter = 0
  }

-- | REPL Monad: 组合 State + Except + IO
type REPL a = StateT REPLState (ExceptT String IO) a

-- | 运行 REPL
runREPL :: REPL a -> IO (Either String a)
runREPL action =
  runExceptT $ evalStateT action initialState

-- ============================================
-- TODO 2: 定义命令类型
-- ============================================

-- | REPL 命令
data Command
  = Eval Expr              -- ^ 求值表达式
  | TypeOf Expr            -- ^ 查看类型
  | Def String Expr        -- ^ 定义: let x = expr
  | Load FilePath          -- ^ 加载文件
  | ShowHistory            -- ^ 显示历史
  | Help                   -- ^ 帮助
  | Quit                   -- ^ 退出
  deriving (Show, Eq)

-- ============================================
-- TODO 3: 实现命令解析
-- ============================================

-- | 解析输入为命令
parseCommand :: String -> REPL Command
parseCommand input
  | ":t " `isPrefixOf` input = do
      let exprStr = drop 3 input
      case parseFullExpr exprStr of
        Left err -> throwError $ "Parse error: " ++ errorBundlePretty err
        Right expr -> return $ TypeOf expr

  | ":load " `isPrefixOf` input = do
      let file = drop 6 input
      return $ Load file

  | input `elem` [":quit", ":q", ":exit"] = return Quit
  | input `elem` [":help", ":h", "?"] = return Help
  | input == ":history" = return ShowHistory

  | "let " `isPrefixOf` input = TODO  -- TODO: 解析 let x = expr

  | otherwise = do
      case parseFullExpr input of
        Left err -> throwError $ "Parse error: " ++ errorBundlePretty err
        Right expr -> return $ Eval expr

-- ============================================
-- TODO 4: 实现命令执行
-- ============================================

-- | 执行命令
executeCommand :: Command -> REPL ()
executeCommand = TODO

-- 提示: 实现以下情况
--
--   Eval expr:
--     1. 获取当前环境的类型
--     2. 类型检查: infer typeEnv expr
--     3. 求值: eval expr
--     4. 显示结果
--     5. 如果是 Lambda,也显示类型
--
--   TypeOf expr:
--     1. 类型检查: infer typeEnv expr
--     2. 显示: "expr : type"
--
--   Def x expr:
--     1. 类型检查: infer typeEnv expr
--     2. 求值: eval expr
--     3. 更新状态: typeEnv <- insert x t typeEnv
--                 valueEnv <- insert x v valueEnv
--     4. 显示: "Defined x : type"
--
--   Load file:
--     1. 读取文件内容
--     2. 逐行执行
--     3. 显示结果或错误
--
--   ShowHistory:
--     1. 显示历史记录 (reverse history)
--
--   Help:
--     1. 显示帮助信息 (调用 showHelp)
--
--   Quit:
--     1. 抛出 "Goodbye!" 异常以退出循环

-- ============================================
-- TODO 5: 实现 REPL 主循环
-- ============================================

-- | REPL 主循环
repl :: REPL ()
repl = do
  st <- get
  liftIO $ putStr $ "λ" ++ show (counter st) ++ "> "
  liftIO $ hFlush stdout
  input <- liftIO getLine

  when (not $ null input) $ do
    modify $ \s -> s { counter = counter s + 1 }
    cmd <- parseCommand input
    executeCommand cmd >> repl

-- ============================================
-- 辅助函数
-- ============================================

-- | 绑定变量到环境
bindVar :: String -> Type -> Expr -> REPL ()
bindVar x t v = modify $ \st ->
  st { typeEnv = Map.insert x t (typeEnv st)
    , valueEnv = Map.insert x v (valueEnv st)
    }

-- | 显示类型
showType :: Expr -> Type -> REPL ()
showType expr t = do
  liftIO $ putStrLn $ prettyPrintStr expr ++ " : " ++ prettyPrintType t

-- | 显示帮助
showHelp :: REPL ()
showHelp = liftIO $ putStrLn $ unlines
  [ "Lambda Calculus Interpreter - Commands:"
  , "  <expr>       Evaluate expression"
  , "  :t <expr>    Show type of expression"
  , "  let x = expr Define variable"
  , "  :load file   Load and execute file"
  , "  :history     Show command history"
  , "  :quit        Exit interpreter"
  , "  :help        Show this help"
  , ""

-- | 加载文件
loadFile :: FilePath -> REPL ()
loadFile path = do
  content <- liftIO $ readFile path
  let lines' = lines content
  liftIO $ putStrLn $ "Loading " ++ path ++ "..."
  mapM_ executeLine (filter (not . null) lines')
  where
    executeLine line
      | head line == '#' = return ()  -- 跳过注释
      | otherwise = do
          modify $ \st -> st { counter = counter st + 1 }
          cmd <- parseCommand line
          executeCommand cmd

-- ============================================
-- 导入的辅助函数
-- ============================================

-- 从 AST 导入
import qualified AST

prettyPrintStr :: Expr -> String
prettyPrintStr = AST.prettyPrintStr

-- 从 TypeChecker 导入
prettyPrintType :: Type -> String
prettyPrintType = TypeChecker.prettyPrintType

-- 从 Parser 导入
import Text.Megaparsec (errorBundlePretty)
import Text.Megaparsec (ParseErrorBundle)
import Data.Void (Void)

parseFullExpr :: String -> Either (ParseErrorBundle String Void) Expr
parseFullExpr = Parser.parseFullExpr

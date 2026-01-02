# Phase 6: REPL 与测试 (REPL & Testing)

## 学习目标

- 理解 Monad Transformer 的使用
- 构建交互式 REPL
- 编写 QuickCheck 属性测试
- 集成所有组件

---

## 6.1 Monad Transformer 入门

### 6.1.1 为什么需要 Transformer?

REPL 需要同时处理多种**效果**:

| 效果 | 用途 | Monad |
|------|------|-------|
| 可变状态 | 类型环境、值环境 | State |
| 错误处理 | 解析/类型错误 | Except |
| IO | 用户交互 | IO |
| 日志 | 记录历史 | Writer |

### 6.1.2 Monad Transformer 栈

```haskell
-- 我们需要的组合:
--   StateT REPLState (ExceptT String IO) a
--   ^^^^^^^^^^^^^^^  ^^^^^^^^^^^^^^^^^
--   可变状态          错误处理 + IO

type REPL a = StateT REPLState (ExceptT String IO) a

-- 读取状态
getState :: REPL REPLState

-- 修改状态
putState :: REPLState -> REPL ()

-- 抛出错误
throwError :: String -> REPL a

-- 执行 IO
liftIO :: IO a -> REPL a
```

### 6.1.3 解释 Transformer 栈

```
┌─────────────────────────────────────┐
│           StateT                    │  ← 可变状态 (REPLState)
│  ┌─────────────────────────────────┐ │
│  │        ExceptT String           │ │  ← 错误处理 (String)
│  │  ┌─────────────────────────────┐ │ │
│  │  │            IO               │ │ │  ← IO 操作
│  │  │  ┌─────────────────────────┐ │ │ │
│  │  │  │     你的返回值 a         │ │ │ │
│  │  │  └─────────────────────────┘ │ │ │
│  │  └─────────────────────────────┘ │ │
│  └─────────────────────────────────┘ │
└─────────────────────────────────────┘
```

### 6.1.4 运行 Transformer

```haskell
-- 初始状态
initialState :: REPLState
initialState = REPLState
  { typeEnv = TypeChecker.emptyEnv
  , valueEnv = Map.empty
  , history = []
  }

-- 运行 REPL
runREPL :: REPL a -> IO (Either String a)
runREPL action =
  runExceptT $ evalStateT action initialState
```

---

## 6.2 REPL 状态

### 6.2.1 定义状态

```haskell
data REPLState = REPLState
  { typeEnv  :: TypeChecker.TypeEnv    -- 类型环境: 变量 -> 类型
  , valueEnv :: Map String Expr        -- 值环境: 变量 -> 值
  , history  :: [String]               -- 命令历史
  , counter  :: Int                    -- 命令计数器
  } deriving (Show)
```

### 6.2.2 状态操作

```haskell
-- 查找变量
lookupVar :: String -> REPL (Type, Expr)
lookupVar x = do
  st <- get
  case Map.lookup x (typeEnv st) of
    Nothing -> throwError $ "Unbound variable: " ++ x
    Just t -> case Map.lookup x (valueEnv st) of
      Nothing -> throwError $ "No value for: " ++ x
      Just v -> return (t, v)

-- 绑定变量
bindVar :: String -> Type -> Expr -> REPL ()
bindVar x t v = modify $ \st ->
  st { typeEnv = Map.insert x t (typeEnv st)
    , valueEnv = Map.insert x v (valueEnv st)
    }
```

---

## 6.3 REPL 命令

### 6.3.1 命令类型

```haskell
data Command
  = Expr Expr              -- 求值表达式
  | TypeOf Expr            -- 查看类型
  | Def String Expr        -- 定义变量: let x = expr
  | Load FilePath          -- 加载文件
  | Quit                   -- 退出
  | Help                   -- 帮助
  | ShowHistory            -- 显示历史
```

### 6.3.2 解析命令

```haskell
-- TODO: 实现命令解析
--
-- :type expr  - 查看类型
-- :load file  - 加载文件
-- :quit 或 :q - 退出
-- :help 或 :h - 帮助
-- :history   - 历史记录

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

  | "let " `isPrefixOf` input = TODO  -- 解析 let x = expr

  | otherwise = do  -- 普通表达式
      case parseFullExpr input of
        Left err -> throwError $ "Parse error: " ++ errorBundlePretty err
        Right expr -> return $ Expr expr
```

---

## 6.4 REPL 主循环

### 6.4.1 主循环结构

```haskell
-- TODO: 实现 REPL 主循环
--
-- 1. 显示提示符 (λ> )
-- 2. 读取输入
-- 3. 解析命令
-- 4. 执行命令
-- 5. 显示结果
-- 6. 更新历史
-- 7. 循环

repl :: REPL ()
repl = do
  liftIO $ putStr "λ> "
  liftIO $ hFlush stdout
  input <- liftIO getLine

  when (not $ null input) $ do
    modify $ \st -> st { counter = counter st + 1 }
    cmd <- parseCommand input
    executeCommand cmd
    repl
```

### 6.4.2 执行命令

```haskell
executeCommand :: Command -> REPL ()
executeCommand = TODO

-- 提示:
--   Expr e:
--     1. 类型检查: infer env e
--     2. 求值: eval e
--     3. 显示结果
--
--   TypeOf e:
--     1. 类型检查: infer env e
--     2. 显示类型
--
--   Def x e:
--     1. 类型检查: infer env e
--     2. 求值: eval e
--     3. 绑定: bindVar x type value
--
--   Load file:
--     1. 读取文件
--     2. 逐行执行
--
--   ShowHistory:
--     1. 显示历史记录
--
--   Help:
--     1. 显示帮助信息
--
--   Quit:
--     1. 退出循环 (使用 ExceptT throwError 或 liftIO exitSuccess)
```

---

## 6.5 QuickCheck 属性测试

### 6.5.1 什么是属性测试?

传统测试: 检查特定输入的输出

```haskell
-- 单元测试
prop_Add_example :: Bool
prop_Add_example =
  eval (Add (LitInt 2) (LitInt 3)) == LitInt 5
```

属性测试: 检查**所有**输入都满足的性质

```haskell
-- 属性测试
prop_Add_commutative :: Expr -> Expr -> Bool
prop_Add_commutative e1 e2 =
  eval (Add e1 e2) == eval (Add e2 e1)
```

### 6.5.2 编写属性

```haskell
-- TODO: 实现属性测试
--
-- 属性 1: 类型保持
--   求值不应该改变表达式的类型
prop_typePreservation :: Expr -> Bool
prop_typePreservation expr =
  case infer emptyEnv expr of
    Left _ -> True  -- 类型错误的表达式跳过
    Right t ->
      case infer emptyEnv (eval expr) of
        Left _ -> False  -- 求值后类型错误!
        Right t' -> t == t'

-- 属性 2: 值的幂等性
--   如果 e 已经是值,eval e == e
prop_valueIdempotent :: Expr -> Property
prop_valueIdempotent expr =
  isValue expr ==> eval expr == expr

-- 属性 3: 加法结合律
--   (a + b) + c == a + (b + c)
prop_addAssociative :: Expr -> Expr -> Expr -> Property
prop_addAssociative a b c =
  isInt a && isInt b && isInt c ==>
    eval (Add (Add a b) c) == eval (Add a (Add b c))

-- 辅助函数
isInt :: Expr -> Bool
isInt (LitInt _) = True
isInt _ = False
```

### 6.5.3 生成器

```haskell
-- TODO: 为 Expr 实现 Arbitrary 实例
--
-- QuickCheck 需要能够生成随机表达式

instance Arbitrary Expr where
  arbitrary = TODO
  -- 提示:
  --   使用 sized 和 frequency 控制生成大小和分布
  --   sized :: (Int -> Gen a) -> Gen a
  --   frequency :: [(Int, Gen a)] -> Gen a

  shrink = TODO  -- 可选: 缩小测试用例

-- 实现示例:
genExpr :: Int -> Gen Expr
genExpr n
  | n == 0 = oneof
      [ LitInt <$> arbitrary
      , LitBool <$> arbitrary
      , Var <$> elements ["x", "y", "z", "f", "g"]
      ]
  | otherwise = frequency
      [ (3, genExpr 0)  -- 字面量
      , (2, Lam <$> elements ["x", "y", "z"] <*> genExpr (n `div` 2))
      , (2, App <$> genExpr (n `div` 2) <*> genExpr (n `div` 2))
      , (1, Add <$> genExpr (n `div` 2) <*> genExpr (n `div` 2))
      ]
```

---

## 6.6 代码骨架

打开 `src/REPL.hs` 并完成以下 TODO:

```haskell
module REPL
  ( -- * REPL 状态
    REPLState(..)
  , initialState
    -- * REPL Monad
  , REPL
  , runREPL
    -- * 主循环
  , repl
    -- * 命令
  , Command(..)
  , executeCommand
    -- * 辅助函数
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
import Data.Maybe (isJust)
import Data.List (isPrefixOf)

-- ============================================
-- TODO 1: 定义 REPL 状态
-- ============================================

data REPLState = REPLState
  { typeEnv  :: TypeEnv
  , valueEnv :: Map String Expr
  , history  :: [String]
  , counter  :: Int
  } deriving (Show)

initialState :: REPLState
initialState = REPLState
  { typeEnv = TypeChecker.emptyEnv
  , valueEnv = Map.empty
  , history = []
  , counter = 0
  }

-- REPL Monad: 组合 State + Except + IO
type REPL a = StateT REPLState (ExceptT String IO) a

-- 运行 REPL
runREPL :: REPL a -> IO (Either String a)
runREPL action =
  runExceptT $ evalStateT action initialState

-- ============================================
-- TODO 2: 实现命令解析
-- ============================================

data Command
  = Eval Expr              -- 求值表达式
  | TypeOf Expr            -- 查看类型
  | Def String Expr        -- 定义: let x = expr
  | Load FilePath          -- 加载文件
  | ShowHistory            -- 显示历史
  | Help                   -- 帮助
  | Quit                   -- 退出
  deriving (Show, Eq)

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
-- TODO 3: 实现命令执行
-- ============================================

executeCommand :: Command -> REPL ()
executeCommand = TODO

-- 提示:
--   Eval expr:
--     1. 类型检查: infer env expr
--     2. 求值: eval expr
--     3. 显示: prettyPrintStr result
--     4. 如果是 Lambda,显示类型
--
--   TypeOf expr:
--     1. 类型检查: infer env expr
--     2. 显示: "expr : type"
--
--   Def x expr:
--     1. 类型检查: infer env expr
--     2. 求值: eval expr
--     3. 更新状态: bindVar x type value
--
--   Load file:
--     1. 读取文件内容
--     2. 逐行执行
--     3. 显示结果
--
--   ShowHistory:
--     1. 显示历史记录 (reverse history)
--
--   Help:
--     1. 显示帮助信息
--
--   Quit:
--     1. 抛出 "Quit" 异常以退出循环

-- ============================================
-- TODO 4: 实现 REPL 主循环
-- ============================================

repl :: REPL ()
repl = do
  st <- get
  liftIO $ putStr $ "λ" ++ show (counter st) ++ "> "
  liftIO $ hFlush stdout
  input <- liftIO getLine

  when (not $ null input) $ do
    modify $ \s -> s { history = input : history s }
    cmd <- parseCommand input
    executeCommand cmd
    repl

-- ============================================
-- 辅助函数
-- ============================================

-- 绑定变量
bindVar :: String -> Type -> Expr -> REPL ()
bindVar x t v = modify $ \st ->
  st { typeEnv = Map.insert x t (typeEnv st)
    , valueEnv = Map.insert x v (valueEnv st)
    }

-- 显示类型
showType :: Expr -> Type -> REPL ()
showType expr t = do
  liftIO $ putStrLn $ prettyPrintStr expr ++ " : " ++ prettyPrintType t

-- 显示帮助
showHelp :: REPL ()
showHelp = liftIO $ putStrLn $ unlines
  [ "Lambda Calculus Interpreter - Commands:"
  , "  <expr>       Evaluate expression"
  , "  :t <expr>    Show type of expression"
  , "  let x = expr Define variable"
  , "  :load file   Load and execute file"
  , "  :history     Show command history"
  , "  :quit        Exit interpreter"
  , ""

-- 加载文件
loadFile :: FilePath -> REPL ()
loadFile path = do
  content <- liftIO $ readFile path
  let lines' = lines content
  forM_ lines' $ \line -> do
    when (not $ null line || head line == '#') $ do
      modify $ \st -> st { counter = counter st + 1 }
      cmd <- parseCommand line
      executeCommand cmd
```

---

## 6.7 测试代码

打开 `test/Spec.hs` 并完成以下 TODO:

```haskell
module Main where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import AST
import TypeChecker
import Evaluator
import Parser

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Lambda Interpreter"
  [ parserTests
  , typeCheckerTests
  , evaluatorTests
  , propertyTests
  ]

-- ============================================
-- TODO 1: 解析器测试
-- ============================================

parserTests :: TestTree
parserTests = testGroup "Parser"
  [ testCase "Parse variable" $
      parseFullExpr "x" @?= Right (Var "x")

  , testCase "Parse lambda" $
      parseFullExpr "\\x -> x" @?= Right (Lam "x" (Var "x"))

  , testCase "Parse application" $
      parseFullExpr "f x" @?= Right (App (Var "f") (Var "x"))

  , testCase "Parse arithmetic" $
      parseFullExpr "1 + 2" @?= Right (Add (LitInt 1) (LitInt 2))

  -- TODO: 添加更多测试用例
  ]

-- ============================================
-- TODO 2: 类型检查器测试
-- ============================================

typeCheckerTests :: TestTree
typeCheckerTests = testGroup "Type Checker"
  [ testCase "Type of literal" $
      TypeChecker.infer TypeChecker.emptyEnv (LitInt 42)
        @?= Right TInt

  , testCase "Type of lambda" $
      case TypeChecker.infer TypeChecker.emptyEnv (Lam "x" (Var "x")) of
        Right (TFun t1 t2) -> t1 @?= t2
        _ -> assertFailure "Expected function type"

  -- TODO: 添加更多测试用例
  ]

-- ============================================
-- TODO 3: 求值器测试
-- ============================================

evaluatorTests :: TestTree
evaluatorTests = testGroup "Evaluator"
  [ testCase "Eval literal" $
      eval (LitInt 42) @?= LitInt 42

  , testCase "Eval addition" $
      eval (Add (LitInt 1) (LitInt 2)) @?= LitInt 3

  , testCase "Eval beta reduction" $
      eval (App (Lam "x" (Var "x")) (LitInt 5)) @?= LitInt 5

  -- TODO: 添加更多测试用例
  ]

-- ============================================
-- TODO 4: 属性测试
-- ============================================

propertyTests :: TestTree
propertyTests = testGroup "Properties"
  [ QC.testProperty "Type preservation" prop_typePreservation
  , QC.testProperty "Value idempotent" prop_valueIdempotent
  , QC.testProperty "Add commutative" prop_addCommutative
  ]

-- TODO: 实现属性
prop_typePreservation :: Expr -> Bool
prop_typePreservation expr = TODO

prop_valueIdempotent :: Expr -> Property
prop_valueIdempotent expr = TODO

prop_addCommutative :: Expr -> Expr -> Property
prop_addCommutative e1 e2 = TODO
```

---

## 6.8 使用 REPL

```bash
# 启动 REPL
stack exec lambda-interpreter-exe

# 示例会话:
λ0> \x -> x
\x. x : t0 -> t0

λ1> let id = \x -> x
Defined id : t0 -> t0

λ2> id 5
5 : Int

λ3> let add = \x -> \y -> x + y
Defined add : Int -> Int -> Int

λ4> add 1 2
3 : Int

λ5> :t add
add : Int -> Int -> Int

λ6> :quit
```

---

## 下一步

恭喜! 你已经完成了整个 lambda 演算解释器的学习。

### 回顾你学到的内容:

1. **Phase 1**: Haskell 工具链 (Stack, GHC, HLS)
2. **Phase 2**: 代数数据类型 (ADT) 和 AST
3. **Phase 3**: 解析器组合子 (Megaparsec)
4. **Phase 4**: Hindley-Milner 类型推断
5. **Phase 5**: Beta 归约和求值策略
6. **Phase 6**: Monad Transformer 和 REPL

### 下一步学习建议:

1. **扩展语言**: 添加 let rec, 模式匹配, 代数数据类型
2. **编译到字节码**: 实现 stack machine 或 JVM
3. **优化**: 实现闭包转换, hoisting, 连续传递风格
4. **类型系统扩展**: 添加类型类, kind 系统, 依赖类型
5. **并发**: 使用 STM 并行化求值

### 推荐阅读:

- "Type Theory and Functional Programming" by Simon Thompson
- "The Implementation of Functional Programming Languages" by Simon Peyton Jones
- "Pearls of Functional Algorithm Design" by Richard Bird

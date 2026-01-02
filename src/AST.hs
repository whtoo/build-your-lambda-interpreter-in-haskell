{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : AST
-- Description : Abstract Syntax Tree for Lambda Calculus
--
-- This module defines the core data structures for representing
-- lambda calculus expressions with basic arithmetic and conditionals.
--
-- Learning objectives:
--   * Understand Algebraic Data Types (ADT)
--   * Learn to model programming language syntax
--   * Practice pattern matching and recursion

module AST
  ( -- * Expression Types
    Expr(..)
    -- * Type System
  , Type(..)
    -- * Pretty Printing
  , prettyPrint
  , prettyPrintStr
    -- * Analysis Functions
  , size
  , freeVars
    -- * Transformations
  , alphaRename
  ) where

import Data.Set (Set)
import qualified Data.Set as Set
import Prettyprinter
import Data.Maybe (isJust)
import Data.List (intercalate)

-- ============================================
-- TODO 1: 定义 Expr 类型
-- ============================================
--
-- 需要支持以下表达式构造子:
--
--   1. Var String     - 变量引用 (例如: x, y, foo)
--   2. Lam String Expr - Lambda 抽象 (例如: λx. body)
--   3. App Expr Expr  - 函数应用 (例如: f x)
--   4. LitInt Int     - 整数字面量 (例如: 42)
--   5. LitBool Bool   - 布尔字面量 (例如: True, False)
--   6. Add Expr Expr  - 加法 (例如: e1 + e2)
--   7. Mul Expr Expr  - 乘法 (例如: e1 * e2)
--   8. If Expr Expr Expr - 条件表达式 (if c then t else f)
--
-- 示例: 表达式 "(λx. x + 1) 5" 的 AST 表示:
--   App (Lam "x" (Add (Var "x") (LitInt 1))) (LitInt 5)
--

-- TODO: 删除下面的 undefined 并实现 Expr 类型
data Expr
  = Var String        -- TODO: 已实现 - 变量
  | Lam String Expr   -- TODO: 已实现 - Lambda 抽象
  | App Expr Expr     -- TODO: 已实现 - 函数应用
  | LitInt Int        -- TODO: 已实现 - 整数字面量
  | LitBool Bool      -- TODO: 已实现 - 布尔字面量
  | Add Expr Expr     -- TODO: 已实现 - 加法
  | Mul Expr Expr     -- TODO: 已实现 - 乘法
  | If Expr Expr Expr -- TODO: 已实现 - 条件表达式
  deriving (Show, Eq)


-- ============================================
-- TODO 2: 定义 Type 类型
-- ============================================
--
-- 需要支持以下类型构造子:
--
--   1. TInt           - 整数类型
--   2. TBool          - 布尔类型
--   3. TFun Type Type - 函数类型 (a -> b)
--   4. TVar String    - 类型变量 (用于类型推断, 例如 'a, 'b)
--
-- 示例类型:
--   Int               -> TInt
--   Bool              -> TBool
--   Int -> Bool       -> TFun TInt TBool
--   (Int -> Bool) -> Int -> Bool
--                     -> TFun (TFun TInt TBool) (TFun TInt TBool)
--

-- TODO: 删除下面的 undefined 并实现 Type 类型
data Type
  = TInt              -- TODO: 已实现 - 整数类型
  | TBool             -- TODO: 已实现 - 布尔类型
  | TFun Type Type    -- TODO: 已实现 - 函数类型
  | TVar String       -- TODO: 已实现 - 类型变量
  deriving (Show, Eq)


-- ============================================
-- TODO 3: 实现 prettyPrint 函数
-- ============================================
--
-- 将表达式转换为美观的 Doc 格式
--
-- 提示:
--   1. 变量: 直接显示名称
--   2. Lambda: 用 "\" 或 "λ" + 参数名 + "." + body
--   3. 应用: 需要考虑括号 (如果子表达式是 App 或 Lam,需要加括号)
--   4. 字面量: 直接显示值
--   5. 运算符: 需要考虑优先级 (Mul 比 Add 优先级高)
--
-- 例子:
--   prettyPrint (Var "x")       = "x"
--   prettyPrint (Lam "x" (Var "x")) = "\\x. x"
--   prettyPrint (Add (LitInt 1) (LitInt 2)) = "1 + 2"
--

prettyPrint :: Expr -> Doc ann
prettyPrint = go 0
  where
    -- go precedence expr: 带优先级的辅助函数
    -- precedence 0 = 最低优先级 (顶层)
    -- precedence 越高,优先级越高
    go :: Int -> Expr -> Doc ann
    go _ (Var x) = pretty x
    go _ (Lam x body) = "\\" <> pretty x <> ". " <> go 0 body
    go p (App e1 e2) =
      -- 应用: 如果外层优先级高于应用,需要加括号
      let lhs = go 1 e1
          rhs = go 2 e2
      in if p > 0
         then parens (lhs <+> rhs)
         else lhs <+> rhs
    go _ (LitInt n) = pretty n
    go _ (LitBool b) = pretty $ if b then "true" else "false"
    go p (Add e1 e2) =
      let lhs = go 1 e1
          rhs = go 1 e2
      in if p > 1
         then parens (lhs <+> "+" <+> rhs)
         else lhs <+> "+" <+> rhs
    go p (Mul e1 e2) =
      let lhs = go 2 e1
          rhs = go 2 e2
      in if p > 2
         then parens (lhs <+> "*" <+> rhs)
         else lhs <+> "*" <+> rhs
    go p (If c t e) =
      let cond = go 0 c
          trueBranch = go 0 t
          falseBranch = go 0 e
      in if p > 0
         then parens ("if" <+> cond <+> "then" <+> trueBranch <+> "else" <+> falseBranch)
         else "if" <+> cond <+> "then" <+> trueBranch <+> "else" <+> falseBranch

-- ============================================
-- TODO 4: 实现 prettyPrintStr 函数
-- ============================================
--
-- 将 prettyPrint 的结果转换为 String
--
-- 提示: 使用 Prettyprinter 的 renderStrict 函数
--

prettyPrintStr :: Expr -> String
prettyPrintStr = TODO  -- TODO: 实现 (提示: 用 renderStrict . layoutPretty defaultLayoutOptions)


-- ============================================
-- TODO 5: 实现 size 函数
-- ============================================
--
-- 计算 AST 的节点数量 (用于测试和分析)
--
-- 提示:
--   1. 基本情况 (Var, LitInt, LitBool): size = 1
--   2. 单个子表达式 (Lam): size = 1 + size(body)
--   3. 多个子表达式 (App, Add, Mul, If):
--      size = 1 + size(e1) + size(e2) + ...
--
-- 例子:
--   size (Var "x")                           = 1
--   size (Lam "x" (Var "x"))                 = 2
--   size (Add (LitInt 1) (LitInt 2))         = 3
--   size (If (LitBool True) (LitInt 1) (LitInt 0)) = 4
--

size :: Expr -> Int
size (Var _) = 1
size (Lam _ body) = 1 + size body
size (App e1 e2) = 1 + size e1 + size e2
size (LitInt _) = 1
size (LitBool _) = 1
size (Add e1 e2) = 1 + size e1 + size e2
size (Mul e1 e2) = 1 + size e1 + size e2
size (If c t e) = 1 + size c + size t + size e


-- ============================================
-- TODO 6: 实现 freeVars 函数
-- ============================================
--
-- 收集表达式中的所有自由变量
--
-- 自由变量 = 未被 Lambda 绑定的变量
--
-- 提示:
--   1. 定义一个辅助函数 go :: [String] -> Expr -> Set String
--      [String] 是当前作用域中绑定的变量列表
--   2. Var x: 如果 x 在绑定列表中,返回 Set.empty;否则返回 Set.singleton x
--   3. Lam x body: 将 x 添加到绑定列表,递归处理 body
--   4. App, Add, Mul: 合并子表达式的自由变量
--   5. Let (如果有): 类似 Lam
--
-- 例子:
--   freeVars (Var "x")                      = fromList ["x"]
--   freeVars (Lam "x" (Var "x"))            = fromList []  (x 被绑定)
--   freeVars (Lam "x" (Var "y"))            = fromList ["y"] (y 是自由的)
--   freeVars (App (Var "x") (Var "y"))      = fromList ["x", "y"]
--   freeVars (Lam "f" (Lam "x" (App (Var "f") (Var "x"))))
--                                           = fromList []  (f 和 x 都被绑定)
--

freeVars :: Expr -> Set String
freeVars = go []
  where
    go :: [String] -> Expr -> Set String
    go _ _expr = Set.empty  -- TODO: 实现 (提示: 使用 Set.union, Set.member, Set.singleton)


-- ============================================
-- TODO 7: 实现 alphaRename 函数
-- ============================================
--
-- Alpha 转换: 重命名 Lambda 的参数变量
--
-- 目的: 避免变量捕获 (variable capture)
--
-- 例子:
--   alphaRename "x" (Lam "x" (Var "x"))
--     => Lam "x'" (Var "x'")
--
--   alphaRename "x" (Lam "x" (Lam "x" (Var "x")))
--     => Lam "x'" (Lam "x" (Var "x"))
--     (只重命名最外层的 x,内部的 x 不会被重命名,因为它们是不同的作用域)
--
-- 提示:
--   1. 定义一个辅助函数 go :: Int -> Expr -> (Expr, Int)
--      Int 是计数器,用于生成唯一的变量名 (x', x'', x''' 等)
--   2. 当遇到 Lam x body 且 x == oldName 时:
--      - 生成新名称 newName = oldName ++ replicate count '\''
--      - 在 body 中将所有 x 引用替换为 newName
--      - 返回 (Lam newName newBody, count + 1)
--   3. 其他情况: 递归处理子表达式
--
-- 注意: 这是一个简化版本,完整的 alpha conversion 需要处理作用域和捕获
--

alphaRename :: String -> Expr -> Expr
alphaRename oldName = go 0
  where
    go :: Int -> Expr -> Expr
    go _ expr = expr  -- TODO: 实现 (提示: 使用 findFreshName 生成新名称)


-- ============================================
-- 辅助函数
-- ============================================

-- | 生成一个新的变量名
-- findFreshName "x" [] = "x'"
-- findFreshName "x" ["x'", "x''"] = "x'''"
findFreshName :: String -> [String] -> String
findFreshName base existing =
  head $ filter (\n -> n `notElem` existing) candidates
  where
    candidates = base : [base ++ replicate n '\'' | n <- [1..]]

-- | 替换表达式中的变量引用
subst :: String -> String -> Expr -> Expr
subst old newName = go
  where
    go (Var x)
      | x == old = Var newName
      | otherwise = Var x
    go (Lam x body)
      | x == old = Lam x body  -- shadowed, don't substitute
      | otherwise = Lam x (go body)
    go (App e1 e2) = App (go e1) (go e2)
    go (LitInt n) = LitInt n
    go (LitBool b) = LitBool b
    go (Add e1 e2) = Add (go e1) (go e2)
    go (Mul e1 e2) = Mul (go e1) (go e2)
    go (If c t e) = If (go c) (go t) (go e)


-- ============================================
-- 测试用例
-- ============================================

-- | 示例表达式用于测试

-- Church encoding: 0 = λf. λx. x
churchZero :: Expr
churchZero = Lam "f" (Lam "x" (Var "x"))

-- Church encoding: 1 = λf. λx. f x
churchOne :: Expr
churchOne = Lam "f" (Lam "x" (App (Var "f") (Var "x")))

-- Y combinator: λf. (λx. f (x x)) (λx. f (x x))
yCombinator :: Expr
yCombinator =
  Lam "f" (
    App
      (Lam "x" (App (Var "f") (App (Var "x") (Var "x"))))
      (Lam "x" (App (Var "f") (App (Var "x") (Var "x"))))
  )

-- Identity function: λx. x
identity :: Expr
identity = Lam "x" (Var "x")

-- Simple arithmetic: (λx. x + 1) 5
identityApply :: Expr
identityApply = App (Lam "x" (Add (Var "x") (LitInt 1))) (LitInt 5)

-- Conditional: if true then 1 else 0
ifExample :: Expr
ifExample = If (LitBool True) (LitInt 1) (LitInt 0)

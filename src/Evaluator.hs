{-# LANGUAGE BangPatterns #-}

-- |
-- Module      : Evaluator
-- Description : Expression evaluator (Beta reduction)
--
-- This module implements evaluation for lambda calculus expressions
-- using beta reduction.
--
-- Learning objectives:
--   * Understand WHNF vs NF
--   * Implement beta reduction
--   * Learn substitution algorithms

module Evaluator
  ( -- * Evaluation
    eval
  , normalize
    -- * Single-step evaluation
  , step
    -- * Value check
  , isValue
    -- * Substitution
  , substitute
    -- * Debugging
  , runSteps
  , prettySteps
  ) where

import AST
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Maybe (mapMaybe)
import Data.List (intercalate)

-- ============================================
-- TODO 1: 判断表达式是否为值
-- ============================================

-- | 判断表达式是否为值
--
-- 值包括:
--   - 字面量 (LitInt, LitBool)
--   - Lambda 抽象 (Lam)
isValue :: Expr -> Bool
isValue (LitInt _) = True
isValue (LitBool _) = True
isValue (Lam _ _) = True
isValue _ = False

-- ============================================
-- TODO 2: 实现替换函数
-- ============================================

-- | 替换自由变量
-- substitute x arg body: 在 body 中将自由出现的 x 替换为 arg
--
-- 重要: 需要避免变量捕获!
--       如果 arg 包含自由变量 y,而 body 有 Lam y,需要先 alpha 转换
substitute :: String -> Expr -> Expr -> Expr
substitute x arg = go
  where
    go (Var y)
      | x == y = arg
      | otherwise = Var y

    go (Lam y body)
      | x == y = Lam y body  -- 阴影: x 被 Lam 绑定
      | y `Set.member` freeVars arg =
          -- 需要避免捕获: alpha 转换
          let y' = freshName y (allVars body)
              body' = substName y y' body
          in Lam y' (go body')
      | otherwise = Lam y (go body)

    go (App e1 e2) = App (go e1) (go e2)
    go (LitInt n) = LitInt n
    go (LitBool b) = LitBool b
    go (Add e1 e2) = Add (go e1) (go e2)
    go (Mul e1 e2) = Mul (go e1) (go e2)
    go (If c t e) = If (go c) (go t) (go e)

    -- 辅助函数

    -- | 收集所有变量 (自由 + 绑定)
    allVars :: Expr -> Set String
    allVars = TODO
      -- 提示: 使用 Set.union,递归收集

    -- | 替换变量名 (用于 alpha 转换)
    substName :: String -> String -> Expr -> Expr
    substName old new = TODO
      -- 提示: 类似 substitute,但替换所有出现 (包括绑定的)

    -- | 生成新的变量名
    freshName :: String -> Set String -> String
    freshName base used =
      head $ filter (\n -> n `Set.notMember` used) candidates
      where
        candidates = base : [base ++ show n | n <- [1..]]

-- ============================================
-- TODO 3: 实现单步求值
-- ============================================

-- | 执行一步归约
-- 返回 Just e' 如果可以归约
-- 返回 Nothing 如果已经是范式
step :: Expr -> Maybe Expr
step = TODO

-- 提示: 实现以下规则 (按优先级)
--
--   规则 1: Beta 归约
--     (λx. body) arg → substitute x arg body
--
--   规则 2: 函数应用 - 归约左部
--     e1 e2 → e1' e2  (如果 e1 可以归约)
--
--   规则 3: 函数应用 - 归约参数
--     v e2 → v e2'  (如果 e1 是值, e2 可以归约)
--
--   规则 4: 算术运算 - 求值左操作数
--     Add e1 e2 → Add e1' e2
--     Mul e1 e2 → Mul e1' e2
--
--   规则 5: 算术运算 - 求值右操作数
--     Add v1 e2 → Add v1 e2'
--     Mul v1 e2 → Mul v1 e2'
--
--   规则 6: 算术运算 - 计算
--     Add (LitInt n1) (LitInt n2) → LitInt (n1 + n2)
--     Mul (LitInt n1) (LitInt n2) → LitInt (n1 * n2)
--
--   规则 7: If - 求值条件
--     If c t e → If c' t e
--
--   规则 8: If - 选择分支
--     If (LitBool True) t e → t
--     If (LitBool False) t e → e
--
--   规则 9: If - 求值分支
--     If (LitBool b) t e → If (LitBool b) t' e
--     If (LitBool b) t e → If (LitBool b) t e'

-- ============================================
-- TODO 4: 实现大步求值 (Call-by-Value)
-- ============================================

-- | 求值到值 (Call-by-Value: 先求值参数)
eval :: Expr -> Expr
eval = TODO

-- 提示: 实现以下规则
--
--   1. 值直接返回
--   2. App: 先 eval e1,再 eval e2,最后 substitute
--   3. Add/Mul: 先 eval 两个操作数,然后计算
--   4. If: 先 eval 条件,然后 eval 相应分支

-- ============================================
-- TODO 5: 实现完全归约 (范式)
-- ============================================

-- | 归约到范式 (完全求值)
normalize :: Expr -> Expr
normalize e =
  case step e of
    Just e' -> normalize e'
    Nothing -> e

-- ============================================
-- 辅助函数
-- ============================================

-- | 运行多步求值,返回所有中间状态
runSteps :: Int -> Expr -> [Expr]
runSteps n e =
  take (n + 1) $ iterate maybeStep e
  where
    maybeStep expr = case step expr of
      Just e' -> e'
      Nothing -> expr

-- | 美化显示求值步骤
prettySteps :: Expr -> String
prettySteps e =
  unlines $ zipWith (++) (map (\n -> show n ++ ": ") [0..])
                        (map prettyPrintStr (runSteps 100 e))

-- ============================================
-- 测试辅助
-- ============================================

-- | 从 AST 导入
-- (这些函数在 AST.hs 中实现)
import qualified AST

prettyPrintStr :: Expr -> String
prettyPrintStr = AST.prettyPrintStr

freeVars :: Expr -> Set String
freeVars = AST.freeVars

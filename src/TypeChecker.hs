{-# LANGUAGE GADTs #-}

-- |
-- Module      : TypeChecker
-- Description : Hindley-Milner type inference
--
-- This module implements type inference for lambda calculus using
-- the Hindley-Milner algorithm.
--
-- Learning objectives:
--   * Understand type substitutions and unification
--   * Learn the Hindley-Milner algorithm
--   * Implement type environments

module TypeChecker
  ( -- * Type checking
    infer
    -- * Types
  , Type(..)
    -- * Errors
  , TypeError(..)
  , prettyTypeError
    -- * Environment
  , TypeEnv
  , emptyEnv
    -- * Substitutions
  , Substitution
  , apply
  , compose
  ) where

import AST
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Monad.State
import Control.Monad.Except
import Data.List (intercalate)

-- ============================================
-- TODO 1: 定义 TypeError
-- ============================================

-- | 类型错误类型
data TypeError
  = UnificationError Type Type
    -- ^ 无法统一两个类型

  | InfiniteType String Type
    -- ^ 无限类型 (occurs check 失败)

  | UnboundVariable String
    -- ^ 未绑定的变量

  | MismatchError String Type Type
    -- ^ 期望的类型与实际类型不匹配

  | OccursCheckFailed String Type
    -- ^ Occurs check 失败

  deriving (Show, Eq)

-- | 格式化错误信息
prettyTypeError :: TypeError -> String
prettyTypeError = TODO
  -- 提示: 为每种错误情况生成友好的错误信息
  --
  -- 示例:
  --   UnificationError TInt TBool
  --     => "Cannot unify Int with Bool"
  --
  --   UnboundVariable "x"
  --     => "Unbound variable: x"

-- ============================================
-- TODO 2: 实现替换操作
-- ============================================

-- | 类型替换: 类型变量到类型的映射
type Substitution = Map String Type

-- | 应用替换到类型
apply :: Substitution -> Type -> Type
apply = TODO
  -- 提示:
  --   apply s (TVar n) = Map.findWithDefault (TVar n) n s
  --   apply s TInt = TInt
  --   apply s TBool = TBool
  --   apply s (TFun t1 t2) = TFun (apply s t1) (apply s t2)

-- | 应用替换到类型环境
applyEnv :: Substitution -> TypeEnv -> TypeEnv
applyEnv s = fmap (apply s)

-- | 合并两个替换 (compose)
-- compose s1 s2 = s1 ∘ s2 (先应用 s2,再应用 s1)
compose :: Substitution -> Substitution -> Substitution
compose = TODO
  -- 提示:
  --   compose s1 s2 = Map.union (apply s1 <$> s2) s1

-- | 单变量替换
singletonSubst :: String -> Type -> Substitution
singletonSubst = TODO

-- ============================================
-- TODO 3: 实现 occurs 检查
-- ============================================

-- | 检查类型变量是否在类型中自由出现
-- occurs 'a ('a -> 'b) = True
-- occurs 'a ('b -> 'c) = False
occurs :: String -> Type -> Bool
occurs var (TVar v) = v == var
occurs var (TFun t1 t2) = TODO
occurs _ _ = False

-- ============================================
-- TODO 4: 实现类型统一
-- ============================================

-- | 统一两个类型
unify :: Type -> Type -> Either TypeError Substitution
unify (TVar v) t
  | v == t = return Map.empty
  | occurs v t = Left $ InfiniteType v t
  | otherwise = return $ singletonSubst v t

unify t (TVar v) = unify (TVar v) t  -- 对称

unify TInt TInt = return Map.empty
unify TBool TBool = return Map.empty

unify (TFun t1 t2) (TFun t3 t4) = do
  s1 <- unify t1 t3
  s2 <- unify (apply s1 t2) (apply s1 t4)
  return $ compose s2 s1

unify t1 t2 = Left $ UnificationError t1 t2

-- ============================================
-- TODO 5: 实现类型变量生成
-- ============================================

-- | 推断 Monad
type Infer a = StateT Int (Either TypeError) a

-- | 生成新的类型变量
newTypeVar :: Infer Type
newTypeVar = do
  n <- get
  put (n + 1)
  return $ TVar ("t" ++ show n)

-- | 运行推断
runInfer :: Infer a -> Either TypeError a
runInfer m = evalStateT m 0

-- ============================================
-- TODO 6: 实现类型推断
-- ============================================

-- | 类型环境
type TypeEnv = Map String Type

-- | 空环境
emptyEnv :: TypeEnv
emptyEnv = Map.empty

-- | 推断表达式类型
infer :: TypeEnv -> Expr -> Either TypeError Type
infer env = runInfer . go env
  where
    go :: TypeEnv -> Expr -> Infer Type

    -- 变量: 在环境中查找
    go _ (Var x) = case Map.lookup x env of
      Just t -> return t
      Nothing -> lift $ Left $ UnboundVariable x

    -- Lambda 抽象
    go env (Lam x body) = TODO
      -- 提示:
      --   1. 生成新类型变量 paramType
      --   2. 扩展环境 env' = Map.insert x paramType env
      --   3. 推断 body 类型 bodyType
      --   4. 返回 TFun paramType bodyType

    -- 函数应用
    go env (App e1 e2) = TODO
      -- 提示:
      --   1. 推断 e1 类型 funType
      --   2. 推断 e2 类型 argType
      --   3. 生成新类型变量 resultType
      --   4. 统一 funType (TFun argType resultType)
      --   5. 返回 resultType

    -- 字面量
    go _ (LitInt _) = return TInt
    go _ (LitBool _) = return TBool

    -- 二元运算: Add
    go env (Add e1 e2) = TODO
      -- 提示:
      --   1. 推断 e1 类型 t1,统一为 TInt
      --   2. 推断 e2 类型 t2,统一为 TInt
      --   3. 返回 TInt

    -- 二元运算: Mul
    go env (Mul e1 e2) = TODO
      -- 提示: 同 Add

    -- 条件表达式
    go env (If c t e) = TODO
      -- 提示:
      --   1. 推断条件类型 ct,统一为 TBool
      --   2. 推断两个分支类型 tt, et
      --   3. 统一 tt 和 et
      --   4. 返回统一后的类型

-- ============================================
-- 辅助函数
-- ============================================

-- | 查找变量的类型
lookupVar :: TypeEnv -> String -> Infer Type
lookupVar env x = case Map.lookup x env of
  Just t -> return t
  Nothing -> lift $ Left $ UnboundVariable x

-- | 扩展环境
extendEnv :: String -> Type -> TypeEnv -> TypeEnv
extendEnv = Map.insert

-- ============================================
-- 测试辅助
-- ============================================

-- | 美化类型显示
prettyPrintType :: Type -> String
prettyPrintType TInt = "Int"
prettyPrintType TBool = "Bool"
prettyPrintType (TFun t1 t2) =
  case t1 of
    TFun _ _ -> "(" ++ prettyPrintType t1 ++ ") -> " ++ prettyPrintType t2
    _ -> prettyPrintType t1 ++ " -> " ++ prettyPrintType t2
prettyPrintType (TVar v) = "'" ++ v

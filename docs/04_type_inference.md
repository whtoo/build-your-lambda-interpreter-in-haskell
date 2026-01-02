# Phase 4: 类型推断 (Type Inference)

## 学习目标

- 理解 Hindley-Milner 类型推断算法
- 掌握类型统一 (Unification)
- 实现类型环境 (Type Environment)
- 理解类型方案 (Type Scheme) 和泛型

---

## 4.1 为什么需要类型推断?

### 4.1.1 显式类型 vs 类型推断

```haskell
-- 显式类型 (Java 风格)
Int addOne(Int x) {
    return x + 1;
}

-- 类型推断 (Haskell 风格)
addOne x = x + 1
-- 编译器自动推断: addOne :: Int -> Int
```

### 4.1.2 类型推断的好处

| 优势 | 说明 |
|------|------|
| **简洁性** | 不需要写冗长的类型签名 |
| **安全性** | 编译时保证类型正确 |
| **文档性** | 类型签名即文档 |
| **重构友好** | 改代码后类型自动更新 |

---

## 4.2 Hindley-Milner 算法概述

### 4.2.1 算法输入输出

```
输入:
  - 类型环境 Γ (Gamma): 已知变量的类型
  - 表达式 e: 需要推断类型的表达式

输出:
  - 类型替换 S: 类型变量到类型的映射
  - 类型 t: 表达式的类型

记作: Γ ⊢ e : t
```

### 4.2.2 核心概念

#### 类型替换 (Substitution)

```haskell
-- 替换是类型变量到类型的映射
type Substitution = Map String Type

-- 示例: { 'a -> Int, 'b -> Bool }
-- 表示将类型变量 'a 替换为 Int, 'b 替换为 Bool
```

#### 应用替换

```haskell
-- 将替换 S 应用到类型 t
apply :: Substitution -> Type -> Type

-- 示例:
--   S = { 'a -> Int }
--   t = 'a -> 'b
--   apply S t = Int -> 'b
```

#### 类型统一 (Unification)

```haskell
-- 找到使两个类型相等的替换
unify :: Type -> Type -> Either TypeError Substitution

-- 示例:
--   unify ('a -> 'b) (Int -> Bool)
--   => Right { 'a -> Int, 'b -> Bool }
--
--   unify Int Bool
--   => Left "Cannot unify Int with Bool"
```

### 4.2.3 算法规则 (Type Inference Rules)

```
1. 变量 (Variable)
   ───────────────────────  (Var)
   Γ ⊢ x : Γ(x)

   如果环境中有 x : t,则表达式 x 的类型是 t


2. 应用 (Application)
   Γ ⊢ e1 : t1      Γ ⊢ e2 : t2
   ───────────────────────────────  (App)
   Γ ⊢ e1 e2 : 'b    where unify t1 (t2 -> 'b)

   如果 e1 的类型是 t1, e2 的类型是 t2,
   则 e1 e2 的类型是 'b,要求 t1 = t2 -> 'b


3. 抽象 (Abstraction)
   Γ, x:'a ⊢ e : t
   ───────────────────────  (Lam)
   Γ ⊢ λx.e : 'a -> t

   在扩展环境 Γ, x:'a 下,如果 e 的类型是 t,
   则 λx.e 的类型是 'a -> t
```

---

## 4.3 实现类型推断

### 4.3.1 类型错误定义

```haskell
-- TODO: 定义 TypeError 类型
--
-- 可能的错误类型:
--   1. UnificationError: 类型无法统一
--   2. InfiniteTypeError: 无限类型 (x = x -> t)
--   3. UnboundVariableError: 未绑定的变量

data TypeError
  = TODO
  deriving (Show, Eq)
```

### 4.3.2 类型变量生成

```haskell
-- TODO: 实现类型变量生成器
--
-- 提示:
--   使用 State Monad 维护计数器
--   每次调用生成新变量: 'a, 'b, 'c, ...

type Infer a = StateT Int (Either TypeError) a

newTypeVar :: Infer Type
newTypeVar = do
  n <- get
  put (n + 1)
  return $ TVar ("t" ++ show n)
```

### 4.3.3 类型替换操作

```haskell
-- TODO: 实现 apply 函数
--
-- 将替换应用到类型上

apply :: Substitution -> Type -> Type
apply s (TVar n) = TODO  -- 提示: 在 Map 中查找
apply s (TInt) = TInt
apply s (TBool) = TBool
apply s (TFun t1 t2) = TODO  -- 提示: 递归应用

-- TODO: 实现复合替换
-- compose s1 s2 = s1 ∘ s2
-- 先应用 s2,再应用 s1

compose :: Substitution -> Substitution -> Substitution
compose = TODO
```

### 4.3.4 类型统一

```haskell
-- TODO: 实现 unify 函数
--
-- 规则:
--   1. a ~ a: 空替换
--   2. 'a ~ t: 如果 'a 不在 t 中自由出现,返回 {'a -> t}
--   3. t ~ 'a: 同上
--   4. t1 -> t2 ~ t3 -> t4: 先 unify t1 t3,再 unify t2 t4
--   5. 其他: 错误

unify :: Type -> Type -> Either TypeError Substitution
unify t1 t2 = TODO

-- 辅助函数: 检查类型变量是否在类型中自由出现
occurs :: String -> Type -> Bool
occurs var = TODO
```

### 4.3.5 类型推断主算法

```haskell
-- TODO: 实现 infer 函数
--
-- 类型环境: 变量名 -> 类型
type TypeEnv = Map String Type

-- 推断表达式类型
infer :: TypeEnv -> Expr -> Either TypeError Type

-- 提示:
--   1. Var x: 在环境中查找 x 的类型
--   2. Lam x body:
--      - 生成新类型变量 t1
--      - 扩展环境: env' = insert x t1 env
--      - 推断 body 类型 t2
--      - 返回 TFun t1 t2
--   3. App e1 e2:
--      - 推断 e1 类型 t1
--      - 推断 e2 类型 t2
--      - 生成新类型变量 t3
--      - 统一 t1 (TFun t2 t3)
--      - 返回 t3
--   4. LitInt n: 返回 TInt
--   5. LitBool b: 返回 TBool
--   6. Add e1 e2 / Mul e1 e2:
--      - 推断 e1 类型 t1
--      - 推断 e2 类型 t2
--      - 统一 t1 TInt
--      - 统一 t2 TInt
--      - 返回 TInt
--   7. If c t e:
--      - 推断 c 类型 tc
--      - 推断 t 类型 tt
--      - 推断 e 类型 te
--      - 统一 tc TBool
--      - 统一 tt te
--      - 返回 tt
```

---

## 4.4 代码骨架

打开 `src/TypeChecker.hs` 并完成以下 TODO:

```haskell
{-# LANGUAGE GADTs #-}

module TypeChecker
  ( -- * 类型检查
    infer
    -- * 类型
  , Type(..)
    -- * 错误
  , TypeError(..)
    -- * 环境
  , TypeEnv
  , emptyEnv
    -- * 辅助函数
  , prettyTypeError
  ) where

import AST
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Monad.State
import Control.Monad.Except

-- ============================================
-- TODO 1: 定义 TypeError
-- ============================================

data TypeError
  = UnificationError Type Type
  | InfiniteType String Type
  | UnboundVariable String
  | OccursCheckFailed String Type
  deriving (Show, Eq)

-- 格式化错误信息
prettyTypeError :: TypeError -> String
prettyTypeError = TODO

-- ============================================
-- TODO 2: 实现替换操作
-- ============================================

type Substitution = Map String Type

-- 应用替换到类型
apply :: Substitution -> Type -> Type
apply = TODO

-- 应用替换到类型环境
applyEnv :: Substitution -> TypeEnv -> TypeEnv
applyEnv = TODO

-- 合并两个替换 (compose)
-- compose s1 s2 = s1 ∘ s2 (先应用 s2,再应用 s1)
compose :: Substitution -> Substitution -> Substitution
compose = TODO

-- 单变量替换
singletonSubst :: String -> Type -> Substitution
singletonSubst = TODO

-- ============================================
-- TODO 3: 实现类型统一
-- ============================================

-- 检查变量是否在类型中自由出现
-- occurs 'a ('a -> 'b) = True
-- occurs 'a ('b -> 'c) = False
occurs :: String -> Type -> Bool
occurs var (TVar v) = v == var
occurs var (TFun t1 t2) = TODO
occurs var _ = False

-- 统一两个类型
unify :: Type -> Type -> Either TypeError Substitution
unify (TVar v) t
  | v == t = return Map.empty  -- t 和 t 是同一类型
  | occurs v t = Left $ InfiniteType v t  -- 无限类型
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
-- TODO 4: 实现类型变量生成
-- ============================================

type Infer a = StateT Int (Either TypeError) a

-- 生成新的类型变量
newTypeVar :: Infer Type
newTypeVar = do
  n <- get
  put (n + 1)
  return $ TVar ("t" ++ show n)

-- 运行推断
runInfer :: Infer a -> Either TypeError a
runInfer m = evalStateT m 0

-- ============================================
-- TODO 5: 实现类型推断
-- ============================================

type TypeEnv = Map String Type

emptyEnv :: TypeEnv
emptyEnv = Map.empty

-- 推断表达式类型
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
      --   2. 扩展环境 env' = insert x paramType env
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

    -- 二元运算
    go env (Add e1 e2) = TODO
    go env (Mul e1 e2) = TODO
      -- 提示:
      --   1. 推断 e1 和 e2 的类型
      --   2. 统一为 TInt
      --   3. 返回 TInt

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

-- 查找变量的类型
lookupVar :: TypeEnv -> String -> Infer Type
lookupVar env x = case Map.lookup x env of
  Just t -> return t
  Nothing -> lift $ Left $ UnboundVariable x

-- 扩展环境
extendEnv :: String -> Type -> TypeEnv -> TypeEnv
extendEnv = Map.insert
```

---

## 4.5 测试用例

```haskell
-- 测试 1: 基本类型
infer emptyEnv (LitInt 42)
-- => Right TInt

infer emptyEnv (LitBool True)
-- => Right TBool

-- 测试 2: 变量查找
let env = Map.singleton "x" TInt
infer env (Var "x")
-- => Right TInt

-- 测试 3: Lambda 表达式
infer emptyEnv (Lam "x" (Var "x"))
-- => Right (TVar "t0" -> TVar "t0")
-- 或: Right (TFun (TVar "t0") (TVar "t0"))

-- 测试 4: 函数应用
infer emptyEnv (App (Lam "x" (Var "x")) (LitInt 5))
-- => Right TInt

-- 测试 5: 算术表达式
infer emptyEnv (Add (LitInt 1) (LitInt 2))
-- => Right TInt

-- 测试 6: 类型错误
infer emptyEnv (Add (LitInt 1) (LitBool True))
-- => Left (UnificationError TInt TBool)

-- 测试 7: 多态函数 (id)
infer emptyEnv (Lam "x" (Var "x"))
-- => Right (TFun (TVar "t0") (TVar "t0"))
-- 解释: 't0 -> 't0,多态恒等函数

-- 测试 8: 高阶函数
infer emptyEnv (Lam "f" (Lam "x" (App (Var "f") (Var "x"))))
-- => Right (TFun (TFun (TVar "t0") (TVar "t1")) (TFun (TVar "t0") (TVar "t1")))
-- 解释: ('t0 -> 't1) -> 't0 -> 't1

-- 测试 9: 条件表达式
infer emptyEnv (If (LitBool True) (LitInt 1) (LitInt 0))
-- => Right TInt

-- 测试 10: 条件表达式类型错误
infer emptyEnv (If (LitInt 1) (LitBool True) (LitBool False))
-- => Left (UnificationError TBool TInt)
```

---

## 4.6 扩展: Let-Polymorphism

### 4.6.1 为什么需要 Let-Polymorphism?

```haskell
-- 不支持 let-polymorphism:
-- let id = \x -> x in
-- (id 5, id true)
-- 错误! id 被推断为 Int -> Int,无法用于 Bool

-- 支持 let-polymorphism:
-- let id = \x -> x in
-- (id 5, id true)
-- 正确! id 是多态的 ∀a. a -> a
```

### 4.6.2 类型方案 (Type Scheme)

```haskell
-- 类型方案: 泛型变量列表 + 类型
data TypeScheme = Forall [String] Type

-- 示例:
-- ∀a. a -> a
Forall ["a"] (TFun (TVar "a") (TVar "a"))

-- Int -> Bool (非多态)
Forall [] (TFun TInt TBool)
```

### 4.6.3 实现 Let-Polymorphism

```haskell
-- TODO: 实现 generalize 和 instantiate
--
-- generalize: 在环境中自由的类型变量变成泛型
generalize :: TypeEnv -> Type -> TypeScheme
generalize = TODO

-- instantiate: 泛型变量替换为新的类型变量
instantiate :: TypeScheme -> Infer Type
instantiate = TODO
```

---

## 下一步

完成类型检查器后,进入 **Phase 5: 求值器 (Evaluator)**

你将学习:
- 弱头范式 (WHNF) vs 范式 (NF)
- Beta 归约
- 替换算法
- 求值策略 (Call-by-name, Call-by-value)

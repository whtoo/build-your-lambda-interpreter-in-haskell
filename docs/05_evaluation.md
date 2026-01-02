# Phase 5: 求值器 (Evaluator)

## 学习目标

- 理解弱头范式 (WHNF) 和范式 (NF) 的区别
- 掌握 Beta 归约算法
- 实现替换策略
- 理解不同的求值顺序

---

## 5.1 求值策略概述

### 5.1.1 什么是"求值"?

求值 = 将表达式"简化"为值的**过程**

```haskell
-- 表达式
(\x -> x + 1) 5

-- 求值步骤:
(\x -> x + 1) 5
→ 5 + 1        -- Beta 归约: 替换 x 为 5
→ 6            -- 算术求值
```

### 5.1.2 值 (Value) 的定义

在我们的语言中,值包括:

| 表达式 | 是值? | 原因 |
|--------|-------|------|
| `42` | ✓ | 整数字面量 |
| `true` | ✓ | 布尔字面量 |
| `\x -> x` | ✓ | Lambda 表达式 |
| `5 + 1` | ✗ | 可以进一步求值 |
| `f x` | ✗ | 函数应用,可以归约 |

```haskell
-- TODO: 判断表达式是否为值

isValue :: Expr -> Bool
isValue (LitInt _) = True
isValue (LitBool _) = True
isValue (Lam _ _) = True
isValue _ = False
```

---

## 5.2 WHNF vs NF

### 5.2.1 弱头范式 (Weak Head Normal Form)

**定义**: 表达式的最外层构造子是值构造子

```haskell
-- WHNF 示例:
\x -> x + 1         -- WHNF: Lambda 是值的构造子
(1 + 2, 3)          -- WHNF: 对的构造子,虽然 1+2 未求值
\f -> f 5           -- WHNF: Lambda
(5, \x -> x + 1)    -- WHNF: 对的构造子

-- 非 WHNF:
1 + 2               -- 需要求值
(\x -> x) 5         -- 函数应用,需要归约
```

**形式化定义**:
```
e 是 WHNF 当且仅当:
  1. e 是 Lambda 抽象: λx. body
  2. e 是值构造子应用的"骨架"
```

### 5.2.2 范式 (Normal Form)

**定义**: 表达式**完全**求值,无法进一步归约

```haskell
-- NF 示例:
42                  -- NF: 整数字面量
\x -> x             -- NF: Lambda (body 也是 NF)
(3, 5)              -- NF: 对的两个元素都是 NF

-- WHNF 但不是 NF:
\x -> 1 + 2         -- WHNF: Lambda,但 body 不是 NF
(1 + 2, 3)          -- WHNF: 对,但第一个元素不是 NF
\f -> f 5           -- WHNF: Lambda,但 body 不是 NF
```

**关系图**:
```
    所有表达式
         │
         ▼
    WHNF 表达式
         │
         ▼
    NF 表达式 (子集)
```

### 5.2.3 为什么 Haskell 使用 WHNF?

```haskell
-- WHNF 允许惰性求值:
take 1 [1 + 2, 3 + 4, 5 + 6]
→ 1 + 2 : ...
→ 3 : ...

-- 注意: 3+4, 5+6 从未被求值!
-- 这就是惰性: 只求值需要的部分
```

---

## 5.3 Beta 归约

### 5.3.1 归约规则

```
(λx. e1) e2  →  e1[x := e2]
```

**解释**: 将 Lambda 的参数 `x` 在 body `e1` 中的所有**自由出现**替换为 `e2`

### 5.3.2 替换算法

```haskell
-- TODO: 实现替换函数
--
-- substitute x arg body: 在 body 中将 x 的自由出现替换为 arg

substitute :: String -> Expr -> Expr -> Expr

-- 规则:
--   1. 如果变量名不同,递归
--   2. 如果变量被 Lambda 绑定(阴影),停止递归
--   3. 其他情况,递归处理子表达式

substitute x arg (Var y)
  | x == y    = arg          -- 替换!
  | otherwise = Var y        -- 不同变量,保留

substitute x arg (Lam y body)
  | x == y    = Lam y body   -- 阴影: 这个 x 不是我们要替换的
  | otherwise = Lam y (substitute x arg body)

substitute x arg (App e1 e2) =
  App (substitute x arg e1) (substitute x arg e2)

-- ... 其他情况类似
```

### 5.3.3 变量捕获问题

```haskell
-- 错误的替换:
(\x -> \y -> x) y
→ \y -> y  -- 错误! y 被意外捕获

-- 正确的替换 (先 alpha 转换):
(\x -> \y -> x) y
→ (\x -> \z -> x) y  -- 先重命名 y 为 z
→ \z -> y  -- 正确!
```

---

## 5.4 求值器实现

### 5.4.1 小步语义 (Small-Step)

```haskell
-- TODO: 实现小步求值
--
-- 返回: Maybe Expr
--   Just e: 一步归约后的表达式
--   Nothing: 已经是值,无法继续归约

step :: Expr -> Maybe Expr

-- 规则:
--   1. Beta 归约: (λx. body) arg → substitute x arg body
--   2. 应用左部: e1 e2 → e1' e2 (如果 e1 可以归约)
--   3. 应用参数: v e2 → v e2' (如果左部是值,归约参数)

step (App (Lam x body) arg) =
  Just $ substitute x arg body

step (App e1 e2) = case step e1 of
  Just e1' -> Just $ App e1' e2  -- 先归约左部
  Nothing -> case step e2 of
    Just e2' -> Just $ App e1 e2'  -- 再归约参数
    Nothing -> Nothing  -- 两边都无法归约

step (Add (LitInt n1) (LitInt n2)) =
  Just $ LitInt (n1 + n2)

step (Add e1 e2) = case step e1 of
  Just e1' -> Just $ Add e1' e2
  Nothing -> case step e2 of
    Just e2' -> Just $ Add e1 e2'
    Nothing -> Nothing

-- ... 其他情况
```

### 5.4.2 大步语义 (Big-Step)

```haskell
-- TODO: 实现大步求值
--
-- 直接求值到值 (不需要 step)

eval :: Expr -> Expr

-- 规则:
--   1. 值直接返回
--   2. 应用: 先求值函数,再求值参数,最后归约

eval (LitInt n) = LitInt n
eval (LitBool b) = LitBool b
eval (Lam x body) = Lam x body  -- Lambda 已经是值

eval (App e1 e2) =
  case eval e1 of
    Lam x body ->
      let arg = eval e2
      in eval (substitute x arg body)
    _ -> error "Cannot apply non-function"

eval (Add e1 e2) =
  case (eval e1, eval e2) of
    (LitInt n1, LitInt n2) -> LitInt (n1 + n2)
    _ -> error "Type error in addition"

-- ... 其他情况
```

---

## 5.5 求值策略对比

### 5.5.1 Call-by-Value (CBV)

```
策略: 先求值参数,再归约

(λx. x + 1) (1 + 2)
→ (λx. x + 1) 3        -- 先求值参数
→ 3 + 1                -- Beta 归约
→ 4                    -- 求值

特点:
  ✓ 简单直观
  ✓ 不重复求值
  ✗ 不够惰性
```

### 5.5.2 Call-by-Name (CBN)

```
策略: 直接替换参数,不先求值

(λx. x + x) (1 + 2)
→ (1 + 2) + (1 + 2)   -- 直接替换
→ 3 + (1 + 2)         -- 求值第一个 (1+2)
→ 3 + 3               -- 求值第二个 (1+2)
→ 6

特点:
  ✓ 惰性
  ✗ 可能重复求值
```

### 5.5.3 Call-by-Need (Haskell)

```
策略: Call-by-Name + 共享 ( thunk )

(λx. x + x) (1 + 2)
→ let thunk = 1 + 2 in
  thunk + thunk
→ let thunk = 3 in   -- 只求值一次!
  thunk + thunk
→ 3 + 3
→ 6

特点:
  ✓ 惰性
  ✓ 不重复求值 (共享)
```

---

## 5.6 代码骨架

打开 `src/Evaluator.hs` 并完成以下 TODO:

```haskell
module Evaluator
  ( -- * 求值函数
    eval
  , normalize
    -- * 单步求值
  , step
    -- * 判断值
  , isValue
    -- * 替换
  , substitute
    -- * 测试
  , runSteps
  , prettySteps
  ) where

import AST
import qualified Data.Set as Set
import Data.Maybe (mapMaybe)

-- ============================================
-- TODO 1: 判断表达式是否为值
-- ============================================

isValue :: Expr -> Bool
isValue = TODO
-- 提示:
--   LitInt, LitBool, Lam 是值
--   其他都不是

-- ============================================
-- TODO 2: 实现替换函数
-- ============================================

-- 替换自由变量
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
    allVars :: Expr -> Set.Set String
    allVars = TODO  -- 收集所有变量 (自由 + 绑定)

    substName :: String -> String -> Expr -> Expr
    substName old new = TODO  -- 类似 substitute,但替换所有出现

    freshName :: String -> Set.Set String -> String
    freshName base used =
      head $ filter (\n -> n `Set.notMember` used) candidates
      where
        candidates = base : [base ++ show n | n <- [1..]]

-- ============================================
-- TODO 3: 实现单步求值
-- ============================================

-- 执行一步归约
-- 返回 Just e' 如果可以归约
-- 返回 Nothing 如果已经是范式
step :: Expr -> Maybe Expr
step = TODO

-- 提示:
--   规则:
--     1. (λx. body) arg → substitute x arg body
--     2. e1 e2 → e1' e2 (如果 e1 可以归约)
--     3. v e2 → v e2' (如果 e1 是值,e2 可以归约)
--     4. Add/Mul: 先归约左操作数,再归约右操作数
--     5. If: 先归约条件,然后根据结果选择分支

-- ============================================
-- TODO 4: 实现大步求值
-- ============================================

-- 求值到值 (Call-by-Value)
eval :: Expr -> Expr
eval = TODO

-- 提示:
--   1. 值直接返回
--   2. App: 先 eval e1,再 eval e2,最后 substitute
--   3. Add/Mul: 先 eval 两个操作数,然后计算
--   4. If: 先 eval 条件,然后 eval 相应分支

-- ============================================
-- TODO 5: 实现完全归约 (范式)
-- ============================================

-- 归约到范式 (完全求值)
normalize :: Expr -> Expr
normalize e =
  case step e of
    Just e' -> normalize e'
    Nothing -> e

-- ============================================
-- 辅助函数
-- ============================================

-- 运行多步求值,返回所有中间状态
runSteps :: Int -> Expr -> [Expr]
runSteps n e =
  take (n + 1) $ iterate maybeStep e
  where
    maybeStep expr = case step expr of
      Just e' -> e'
      Nothing -> expr

-- 美化显示求值步骤
prettySteps :: Expr -> String
prettySteps e =
  unlines $ zipWith (++) (map (\n -> show n ++ ": ") [0..]) (map prettyPrintStr (runSteps 100 e))
```

---

## 5.7 测试用例

```haskell
-- 测试 1: 基本求值
eval (LitInt 42)
-- => LitInt 42

eval (Add (LitInt 1) (LitInt 2))
-- => LitInt 3

-- 测试 2: Beta 归约
eval (App (Lam "x" (Var "x")) (LitInt 5))
-- => LitInt 5

-- 测试 3: 嵌套 Lambda
eval (App (App (Lam "f" (Lam "x" (App (Var "f") (Var "x")))) (Lam "y" (Add (Var "y") (LitInt 1))) (LitInt 5))
-- => LitInt 6

-- 解释:
--   ((\f -> \x -> f x) (\y -> y + 1)) 5
-- → (\x -> (\y -> y + 1) x) 5
-- → (\y -> y + 1) 5
-- → 5 + 1
-- → 6

-- 测试 4: 条件表达式
eval (If (LitBool True) (LitInt 1) (LitInt 0))
-- => LitInt 1

eval (If (LitBool False) (LitInt 1) (LitInt 0))
-- => LitInt 0

-- 测试 5: 惰性 (如果实现 Call-by-Name)
-- \x -> y (不消耗参数)
let neverTerminating = App (Lam "x" (Var "x")) (App (Lam "x" (App (Var "x") (Var "x"))) (Lam "x" (App (Var "x") (Var "x"))))
-- Call-by-Value: 死循环
-- Call-by-Name: 返回 Lam "x" (Var "x") (不求值参数)

-- 测试 6: 单步求值
step (App (Lam "x" (Add (Var "x") (LitInt 1))) (LitInt 5))
-- => Just (Add (LitInt 5) (LitInt 1))

step (Add (LitInt 5) (LitInt 1))
-- => Just (LitInt 6)

step (LitInt 6)
-- => Nothing (已经是值)

-- 测试 7: Y 组合子 (递归)
-- Y = \f. (\x -> f (x x)) (\x -> f (x x))
--
-- 定义阶乘:
-- fact = Y (\f n -> if n == 0 then 1 else n * f (n - 1))
--
-- 注意: 需要 if-then-else 和布尔比较才能完整实现
```

---

## 5.8 调试求值器

### 使用 `trace` 调试

```haskell
import Debug.Trace

step :: Expr -> Maybe Expr
step e = trace ("step: " ++ prettyPrintStr e) $ case e of
  App (Lam x body) arg ->
    trace ("Beta: substituting " ++ x ++ " with " ++ prettyPrintStr arg) $
    Just $ substitute x arg body
  -- ...
```

### 可视化求值过程

```haskell
-- 显示求值步骤
traceEval :: Expr -> IO ()
traceEval e = go 0 e
  where
    go n expr = do
      putStrLn $ show n ++ ": " ++ prettyPrintStr expr
      case step expr of
        Just expr' -> go (n + 1) expr'
        Nothing -> putStrLn "Done!"
```

---

## 5.9 扩展练习

### 挑战 1: 实现 Call-by-Name

```haskell
-- TODO: 实现按名调用

evalCBN :: Expr -> Expr
evalCBN = TODO
-- 提示: 不先求值参数,直接替换
```

### 挑战 2: 实现 Call-by-Need

```haskell
-- TODO: 使用 thunk 实现按需调用

data Thunk = Forced Expr | Unevaluated Expr

evalCBN :: Expr -> Expr
evalCBN = TODO
-- 提示: 维护一个 thunk 缓存,避免重复求值
```

### 挑战 3: 添加递归

```haskell
-- TODO: 添加 let rec 支持
--
-- let rec fact = \n ->
--   if n == 0 then 1 else n * fact (n - 1)
-- in fact 5

parseLetRec :: Parser Expr
parseLetRec = TODO
```

---

## 下一步

完成求值器后,进入 **Phase 6: REPL 与测试**

你将学习:
- Monad Transformer
- 构建 REPL
- QuickCheck 属性测试
- 集成所有组件

# Phase 2: 抽象语法树 (AST) - 代数数据类型

> 📖 **参考文档**: [语言语法定义与例子](language_syntax.md) - 完整的语法规范和例子

## 学习目标

- 理解什么是代数数据类型 (ADT)
- 掌握如何用 ADT 建模编程语言语法
- 实现表达式类型的 pretty-printer
- 理解 Haskell 类型系统的核心思想

---

## 2.1 什么是代数数据类型 (ADT)?

### 2.1.1 直观理解

代数数据类型 = **"和类型"** + **"积类型"**

| 类型 | 数学表示 | Haskell 语法 | 含义 |
|------|----------|--------------|------|
| 和类型 | `A + B` | `data T = A \| B` | **或**的关系: 是 A **或** 是 B |
| 积类型 | `A × B` | `data T = T A B` | **且**的关系: 同时有 A **且** 有 B |

### 2.1.2 经典例子: Maybe 类型

```haskell
data Maybe a
  = Nothing      -- 和类型分支: 没有值
  | Just a       -- 和类型分支: 有一个值 a (积类型,包含一个值)

-- 使用:
divide :: Int -> Int -> Maybe Int
divide _ 0 = Nothing   -- 除以零,返回 Nothing
divide n d = Just (n `div` d)
```

**为什么叫"代数"?**

Maybe 类型的"基数" (可能的值数量):
- `Maybe Bool` 有 `1 + 2 = 3` 个值: `Nothing`, `Just False`, `Just True`
- `Maybe Int` 有 `1 + ∞ = ∞` 个值

这种"加法"就是代数!

### 2.1.3 递归类型

ADT 可以递归定义,就像自然数:

```haskell
-- 自然数 = 0 或 (自然数 + 1)
data Nat
  = Zero      -- 基础情况
  | Succ Nat  -- 递归情况

-- 3 = Succ (Succ (Succ Zero))
```

---

## 2.2 用 ADT 建模表达式

### 2.2.1 核心思想

编程语言的每个语法规则 → 一个 ADT 构造子

例如算术表达式:
```
expr ::= int                -- 整数字面量
       | expr + expr        -- 加法
       | expr * expr        -- 乘法
       | ( expr )           -- 括号
```

转换为 Haskell ADT:

```haskell
data Expr
  = Lit Int           -- 字面量: 42
  | Add Expr Expr     -- 加法: e1 + e2
  | Mul Expr Expr     -- 乘法: e1 * e2
  deriving (Show, Eq)
```

### 2.2.2 表达式示例

表达式 `(1 + 2) * 3` 的 AST 表示:

```
        Mul
       /   \
     Add    Lit 3
    /   \
Lit 1  Lit 2
```

Haskell 代码:

```haskell
exampleExpr :: Expr
exampleExpr = Mul (Add (Lit 1) (Lit 2)) (Lit 3)
```

### 2.2.3 为什么 AST 优于字符串?

```haskell
-- 字符串表示 (容易出错)
expr1 = "(1 + 2) * 3"
expr2 = "1 + 2 * 3"  -- 优先级错误!

-- AST 表示 (结构清晰)
expr1 = Mul (Add (LitInt 1) (LitInt 2)) (LitInt 3)
expr2 = Add (LitInt 1) (Mul (LitInt 2) (LitInt 3))
```

AST 天然避免了:
- 运算符优先级混淆
- 括号匹配错误
- 词法/语法错误

### 2.2.4 本语言的表达式类型

我们的语言支持以下表达式（详见 [语法定义](language_syntax.md)）：

```haskell
-- 基本表达式
Var "x"                    -- 变量 x
LitInt 42                  -- 整数 42
LitBool True               -- 布尔值 true

-- Lambda 表达式
Lam "x" (Var "x")         -- λx. x (恒等函数)
Lam "x" (Lam "y" (Var "x")) -- λx. λy. x (常数函数)

-- 函数应用
App (Lam "x" (Var "x")) (LitInt 5)  -- (λx. x) 5

-- 算术运算
Add (LitInt 1) (LitInt 2)  -- 1 + 2
Mul (LitInt 3) (LitInt 4)  -- 3 * 4

-- 条件表达式
If (LitBool True) (LitInt 1) (LitInt 0)  -- if true then 1 else 0
```

---

## 2.3 Lambda 演算的 AST

### 2.3.1 Lambda 演算语法

Lambda 演算是最简单的编程语言,只有 3 种表达式:

| 语法 | 名称 | 含义 | Haskell ADT |
|------|------|------|-------------|
| `x` | 变量 | 引用绑定的变量 | `Var String` |
| `λx. e` | 抽象 | 函数定义 | `Lam String Expr` |
| `e1 e2` | 应用 | 函数调用 | `App Expr Expr` |

### 2.3.2 完整表达式类型

```haskell
-- 我们的语言支持的完整表达式类型
-- 参见 [语法定义](language_syntax.md) 获取详细信息

data Expr
  = Var String        -- 变量 (例如: x, foo, myVar)
  | Lam String Expr   -- Lambda 抽象 (例如: \x -> body)
  | App Expr Expr     -- 函数应用 (例如: f x)
  | LitInt Int        -- 整数字面量 (例如: 42, 0, -1)
  | LitBool Bool      -- 布尔字面量 (例如: true, false)
  | Add Expr Expr     -- 加法 (例如: e1 + e2)
  | Mul Expr Expr     -- 乘法 (例如: e1 * e2)
  | If Expr Expr Expr -- 条件表达式 (if c then t else f)
  deriving (Show, Eq)

-- 对应的语法例子:
-- Var "x"           ⟷ x
-- Lam "x" (Var "x") ⟷ \x -> x
-- App (Var "f") (Var "x") ⟷ f x
-- LitInt 42         ⟷ 42
-- LitBool True      ⟷ true
-- Add (LitInt 1) (LitInt 2) ⟷ 1 + 2
-- If (LitBool True) (LitInt 1) (LitInt 0) ⟷ if true then 1 else 0
```

### 2.3.3 Lambda 演算例子

**Church 编码:** 用 Lambda 表示自然数

```haskell
-- 0 = λf. λx. x
churchZero :: Expr
churchZero = Lam "f" (Lam "x" (Var "x"))

-- 1 = λf. λx. f x
churchOne :: Expr
churchOne = Lam "f" (Lam "x" (App (Var "f") (Var "x")))

-- succ = λn. λf. λx. f (n f x)
churchSucc :: Expr
churchSucc =
  Lam "n" (
    Lam "f" (
      Lam "x" (
        App (Var "f")
            (App (App (Var "n") (Var "f")) (Var "x"))
      )
    )
  )

-- Y combinator: λf. (λx. f (x x)) (λx. f (x x))
yCombinator :: Expr
yCombinator =
  Lam "f" (
    App
      (Lam "x" (App (Var "f") (App (Var "x") (Var "x"))))
      (Lam "x" (App (Var "f") (App (Var "x") (Var "x"))))
  )
```

---

## 2.4 类型系统表示

### 2.4.1 简单类型

```haskell
-- TODO: 补全 Type 类型定义

-- 提示: 需要支持:
--   1. 基本类型: TInt, TBool
--   2. 函数类型: TFun Type Type (表示 a -> b)
--   3. 类型变量: TVar String (用于类型推断)

-- 你的实现:
data Type
  = TODO  -- TODO: 实现这个类型定义
  deriving (Show, Eq)
```

### 2.4.2 类型示例

```haskell
-- Int 类型
tInt :: Type
tInt = TInt

-- Bool 类型
tBool :: Type
tBool = TBool

-- Int -> Bool 类型
intToBool :: Type
intToBool = TFun TInt TBool

-- (Int -> Bool) -> Int -> Bool
higherOrder :: Type
higherOrder = TFun (TFun TInt TBool) (TFun TInt TBool)

-- 类型变量 (用于多态函数)
tVar :: Type
tVar = TVar "a"
```

---

## 2.5 Pretty Printing - 美化输出

### 2.5.1 为什么需要?

```haskell
-- 默认 Show 输出 (难读)
Lam "x" (Add (Var "x") (Lit 1))

-- Pretty Print 输出 (可读)
"\\x. x + 1"
```

### 2.5.2 实现 Pretty Print

我们使用 `prettyprinter` 库:

```haskell
-- TODO: 实现 prettyPrint 函数

-- 提示:
--   1. 使用 prettyprinter 的 Doc 构建器
--   2. 变量: 直接显示名称
--   3. Lambda: 用 "\" 或 "λ" + "." 分隔
--   4. 应用: 用空格分隔,可能需要括号
--   5. 字面量: 直接显示数值

import Prettyprinter

prettyPrint :: Expr -> Doc ann
prettyPrint = TODO  -- TODO: 实现这个函数

-- 或者简单版本,返回 String:
prettyPrintStr :: Expr -> String
prettyPrintStr = TODO  -- TODO: 实现这个函数
```

### 2.5.3 测试用例

```haskell
-- 测试 1: 变量
prettyPrint (Var "x")
-- 期望: "x"

-- 测试 2: Lambda
prettyPrint (Lam "x" (Var "x"))
-- 期望: "\\x. x" 或 "λx. x"

-- 测试 3: 应用
prettyPrint (App (Lam "x" (Var "x")) (Lit 5))
-- 期望: "(\\x. x) 5"

-- 测试 4: 嵌套 Lambda
prettyPrint (Lam "f" (Lam "x" (App (Var "f") (Var "x"))))
-- 期望: "\\f. \\x. f x"

-- 测试 5: Y 组合子
prettyPrint yCombinator
-- 期望: "\\f. (\\x. f (x x)) (\\x. f (x x))"
```

---

## 2.6 模式匹配 - ADT 的操作方式

### 2.6.1 基础模式匹配

```haskell
-- 计算表达式的"大小" (节点数量)
size :: Expr -> Int
size (Var _) = 1                    -- 变量大小为 1
size (Lam _ body) = 1 + size body  -- Lambda = 1 + body 大小
size (App e1 e2) = 1 + size e1 + size e2  -- 应用 = 1 + e1 + e2
size (Lit _) = 1                    -- 字面量大小为 1
size (Add e1 e2) = 1 + size e1 + size e2
size (If c t e) = 1 + size c + size t + size e

-- 测试:
-- size (Add (Lit 1) (Lit 2))  = 1 + 1 + 1 = 3
-- size (Lam "x" (Var "x"))    = 1 + 1 = 2
```

### 2.6.2 收集自由变量

**自由变量** = 未被 Lambda 绑定的变量

```haskell
-- TODO: 实现 freeVars 函数

-- 提示:
--   1. Var x: 如果 x 在绑定列表中,则是绑定的;否则是自由的
--   2. Lam x body: x 在 body 中是绑定的
--   3. App e1 e2: 合并 e1 和 e2 的自由变量
--   4. 其他情况: 递归处理子表达式

-- 你的实现:
freeVars :: Expr -> Set.Set String
freeVars = TODO  -- TODO: 实现这个函数

-- 测试用例:
-- freeVars (Var "x")
--   期望: fromList ["x"]
--
-- freeVars (Lam "x" (Var "x"))
--   期望: fromList []  -- x 被绑定了
--
-- freeVars (Lam "x" (Var "y"))
--   期望: fromList ["y"]  -- y 是自由的
--
-- freeVars (App (Var "x") (Var "y"))
--   期望: fromList ["x", "y"]
```

### 2.6.3 Alpha 转换 (变量重命名)

为了避免"变量捕获",我们需要重命名:

```haskell
-- TODO: 实现 alphaRename 函数

-- 提示:
--   1. 生成新名称 (例如 x' 如果原名称是 x)
--   2. 递归更新表达式中的所有引用

-- 你的实现:
alphaRename :: String -> Expr -> Expr
alphaRename oldName = TODO  -- TODO: 实现这个函数

-- 测试用例:
-- alphaRename "x" (Lam "x" (Var "x"))
--   期望: Lam "x'" (Var "x'")
```

---

## 2.7 代码骨架

现在打开 `src/AST.hs` 并完成以下 TODO:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module AST
  ( Expr(..)
  , Type(..)
  , prettyPrint
  , prettyPrintStr
  , size
  , freeVars
  , alphaRename
  ) where

import Data.Set (Set)
import qualified Data.Set as Set
import Prettyprinter

-- TODO 1: 定义 Expr 类型
-- 需要支持: Var, Lam, App, Lit Int, Lit Bool, Add, Sub, Mul, If
data Expr = TODO
  deriving (Show, Eq)

-- TODO 2: 定义 Type 类型
-- 需要支持: TInt, TBool, TFun, TVar
data Type = TODO
  deriving (Show, Eq)

-- TODO 3: 实现 prettyPrint (返回 Doc)
prettyPrint :: Expr -> Doc ann
prettyPrint = TODO

-- TODO 4: 实现 prettyPrintStr (返回 String)
prettyPrintStr :: Expr -> String
prettyPrintStr = TODO

-- TODO 5: 实现 size (计算 AST 节点数)
size :: Expr -> Int
size = TODO

-- TODO 6: 实现 freeVars (收集自由变量)
freeVars :: Expr -> Set String
freeVars = TODO

-- TODO 7: 实现 alphaRename (变量重命名)
alphaRename :: String -> Expr -> Expr
alphaRename = TODO
```

---

## 2.8 验证你的实现

### 编译检查

```bash
stack build
```

### GHCi 测试

```bash
stack ghci
```

```haskell
-- 加载你的模块
:load src/AST.hs

-- 测试 1: 创建表达式
let e = Add (Lit 1) (Lit 2)
size e
-- 期望: 3

-- 测试 2: Pretty print
prettyPrintStr (Lam "x" (Add (Var "x") (Lit 1)))
-- 期望: "\\x. x + 1"

-- 测试 3: 自由变量
import qualified Data.Set as Set
freeVars (Lam "x" (Add (Var "x") (Var "y")))
-- 期望: fromList ["y"]

-- 测试 4: 复杂表达式
let yComb = Lam "f" (App (Lam "x" (App (Var "f") (App (Var "x") (Var "x")))) (Lam "x" (App (Var "f") (App (Var "x") (Var "x")))))
prettyPrintStr yComb
-- 期望: "\\f. (\\x. f (x x)) (\\x. f (x x))"
```

---

## 2.9 扩展练习

### 挑战 1: 添加 Let 绑定

扩展 `Expr` 类型支持 `let` 表达式:

```haskell
-- let x = 5 in x + 1
data Expr
  = ...
  | Let String Expr Expr  -- Let x value body
```

实现 `desugarLet` 函数,将 `let` 转换为 Lambda:

```haskell
-- let x = e1 in e2  ===>  (\x. e2) e1
desugarLet :: Expr -> Expr
desugarLet = TODO
```

### 挑战 2: 添加类型注解

扩展 `Expr` 支持类型注解 (用于显式类型):

```haskell
data Expr
  = ...
  | Ann Expr Type  -- e :: t
```

例如 `(λx. x) :: Int -> Int`:

```haskell
example :: Expr
example = Ann (Lam "x" (Var "x")) (TFun TInt TInt)
```

---

## 下一步

完成 AST 模块后,进入 **Phase 3: 解析器组合子**

你将学习:
- 什么是解析器组合子
- 如何用 Megaparsec 编写解析器
- 如何处理词法和语法分析

# 语言语法定义与例子

## 概述

本项目实现了一个支持类型推断的 Lambda 演算方言，结合了函数式编程的基本特性：
- **Lambda 演算核心**：变量、抽象、应用
- **基本类型**：整数、布尔值
- **算术运算**：加法、乘法
- **条件表达式**：if-then-else
- **类型推断**：基于 Hindley-Milner 算法

## 语法定义 (EBNF)

### 词法规则

```ebnf
letter     = "a" | "b" | ... | "z" | "A" | "B" | ... | "Z" ;
digit      = "0" | "1" | ... | "9" ;
ident      = letter { letter | digit | "_" } ;
integer    = digit { digit } ;
whitespace = " " | "\t" | "\n" | "\r" ;

(* 关键字 *)
keyword    = "if" | "then" | "else" | "true" | "false" | "lambda" ;

(* 标识符不能是关键字 *)
varname   = ident - keyword ;
```

### 语法规则

```ebnf
expr       = if_expr | lambda_expr | binop_expr ;

if_expr    = "if" expr "then" expr "else" expr ;

lambda_expr = "\\" varname "->" expr  (* Lambda 抽象 *)
            | "lambda" varname "." expr ;

binop_expr = term { ( "+" | "*" ) term } ;

term       = application ;

application = atom { atom } ;  (* 左结合 *)

atom       = varname
           | integer
           | "true" | "false"
           | "(" expr ")" ;
```

### 运算符优先级

从高到低：
1. **函数应用**：`f x y` ≡ `(f x) y`
2. **乘法**：`a * b`
3. **加法**：`a + b`
4. **条件表达式**：`if c then t else f`

## 类型系统

### 基本类型

```ebnf
type       = type_atom [ "->" type ] ;

type_atom  = "Int" | "Bool" | type_var | "(" type ")" ;

type_var   = "'" letter { letter | digit } ;
```

### 类型规则

| 表达式 | 类型 |
|--------|------|
| `n` (整数) | `Int` |
| `true` / `false` | `Bool` |
| `x` (变量) | 由环境决定 |
| `\\x. e` | `T₁ → T₂` (如果 `e: T₂` 且 `x: T₁`) |
| `f x` | `T₂` (如果 `f: T₁ → T₂` 且 `x: T₁`) |
| `e₁ + e₂` | `Int` (如果 `e₁: Int` 且 `e₂: Int`) |
| `e₁ * e₂` | `Int` (如果 `e₁: Int` 且 `e₂: Int`) |
| `if c then t else f` | `T` (如果 `c: Bool` 且 `t: T` 且 `f: T`) |

## 例子程序

### 基本表达式

```haskell
-- 整数字面量
42

-- 布尔字面量  
true
false

-- 变量
x
y
foo
```

### Lambda 表达式

```haskell
-- 恒等函数: λx. x
\\x -> x

-- 带类型的恒等函数: (λx: Int. x): Int → Int
\\x -> x :: Int -> Int

-- 常数函数: λx. λy. x  
\\x -> \\y -> x

-- 函数组合: λf. λg. λx. f (g x)
\\f -> \\g -> \\x -> f (g x)
```

### 函数应用

```haskell
-- 简单应用
(\\x -> x) 5

-- 多参数应用 (柯里化)
(\\f -> \\x -> f x) (\\y -> y + 1) 10

-- 等价于: (λf. λx. f x) (λy. y + 1) 10
-- 结果: 11
```

### 算术运算

```haskell
-- 基本运算
1 + 2
3 * 4

-- 运算符优先级
1 + 2 * 3      -- = 1 + (2 * 3) = 7
(1 + 2) * 3    -- = 3 * 3 = 9

-- 在 Lambda 中使用
\\x -> x + 1
\\x -> \\y -> x * y + 1
```

### 条件表达式

```haskell
-- 基本条件
if true then 1 else 0

-- 条件中的运算
if 1 + 1 == 2 then true else false

-- 在 Lambda 中使用
\\x -> if x > 0 then x else 0

-- 嵌套条件
if true then (if false then 1 else 2) else 3
```

### 复杂例子

#### 1. 阶乘函数 (使用 Y 组合子)

```haskell
-- Y 组合子
Y = \\f -> (\\x -> f (x x)) (\\x -> f (x x))

-- 阶乘函数
fact = Y (\\f -> \\n -> 
  if n == 0 then 1 else n * f (n - 1))

-- 使用
fact 5  -- = 120
```

#### 2. 列表处理 (Church 编码)

```haskell
-- 列表的基本操作
-- cons = λx. λxs. λc. c x xs
cons = \\x -> \\xs -> \\c -> c x xs

-- nil = λc. c
nil = \\c -> c

-- head = λxs. xs (λx. λxs. x)  
head = \\xs -> xs (\\x -> \\xs -> x)

-- tail = λxs. xs (λx. λxs. xs)
tail = \\xs -> xs (\\x -> \\xs -> xs)
```

#### 3. 布尔运算 (Church 编码)

```haskell
-- 基本布尔值
true  = \\t -> \\f -> t
false = \\t -> \\f -> f

-- 布尔运算
and = \\p -> \\q -> p q p
or  = \\p -> \\q -> p p q
not = \\p -> \\t -> \\f -> p f t

-- 使用
and true false  -- = false
or true false   -- = true  
not true        -- = false
```

#### 4. 算术运算 (Church 编码)

```haskell
-- Church 数: 0 = λf. λx. x
zero = \\f -> \\x -> x
one  = \\f -> \\x -> f x
two  = \\f -> \\x -> f (f x)

-- 后继函数
succ = \\n -> \\f -> \\x -> f (n f x)

-- 加法
add = \\m -> \\n -> \\f -> \\x -> m f (n f x)

-- 乘法  
mul = \\m -> \\n -> \\f -> m (n f)

-- 使用
add one two  -- = three
mul two three -- = six
```

## 类型推断例子

### 基本类型推断

```haskell
-- 字面量
42       -- 类型: Int
true     -- 类型: Bool

-- 变量 (需要环境)
x        -- 类型: 取决于 x 的类型
```

### Lambda 类型推断

```haskell
-- 恒等函数
\\x -> x          -- 类型: 'a -> 'a

-- 常数函数  
\\x -> \\y -> x    -- 类型: 'a -> 'b -> 'a

-- 函数组合
\\f -> \\g -> \\x -> f (g x)  -- 类型: ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c
```

### 应用类型推断

```haskell
-- 简单应用
(\\x -> x + 1) 5  -- 类型: Int

-- 多态应用
(\\f -> f true) (\\x -> x)  -- 类型: Bool
```

### 条件表达式类型推断

```haskell
-- 基本条件
if true then 1 else 0      -- 类型: Int

-- 多态条件
if true then \\x -> x else \\x -> x + 1  -- 类型: Int -> Int
```

## 语法糖

### 1. Let 绑定 (可去糖化)

```haskell
-- 语法糖
let x = 5 in x + 1

-- 去糖化后
(\\x -> x + 1) 5
```

### 2. 多参数 Lambda

```haskell
-- 语法糖
\\x y z -> x + y + z

-- 去糖化后  
\\x -> \\y -> \\z -> x + y + z
```

### 3. 中缀运算符

```haskell
-- 语法糖
1 + 2 * 3

-- 去糖化后
Add (LitInt 1) (Mul (LitInt 2) (LitInt 3))
```

## 错误消息

### 类型错误

```haskell
-- 错误: 期望 Int, 得到 Bool
if 1 then 2 else 3

-- 错误: 类型不匹配  
(\\x -> x + 1) true

-- 错误: 无限类型
\\x -> x x
```

### 语法错误

```haskell
-- 错误: 未闭合的括号
(\\x -> x

-- 错误: 期望关键字
if true 1 else 2

-- 错误: 未绑定变量
undefined_var
```

## 与其他语言的比较

### 与 Haskell 的比较

| 特性 | 本语言 | Haskell |
|------|--------|---------|
| Lambda 语法 | `\\x -> e` | `\\x -> e` |
| 类型注解 | `e :: t` | `e :: t` |
| 条件表达式 | `if c then t else f` | `if c then t else f` |
| 基本运算 | `+`, `*` | `+`, `*` |
| 递归 | Y 组合子 | `let rec` / 显式递归 |
| 多态 | Hindley-Milner | Hindley-Milner + 扩展 |
| 类型类 | 无 | 有 |

### 与 ML 的比较

| 特性 | 本语言 | ML |
|------|--------|-----|
| Lambda 语法 | `\\x -> e` | `fn x => e` |
| 类型推断 | Hindley-Milner | Hindley-Milner |
| 递归 | Y 组合子 | `let rec` |
| 多态 | 有 | 有 |

### 与 Lisp 的比较

| 特性 | 本语言 | Lisp |
|------|--------|------|
| 语法 | 中缀 | 前缀 |
| 类型 | 静态类型 | 动态类型 |
| Lambda | `\\x -> e` | `(lambda (x) e)` |

## 实现注意事项

### 1. 作用域规则

- **词法作用域**：变量绑定在定义时确定
- **静态绑定**：Lambda 参数创建新的作用域
- **变量捕获**：需要 alpha 转换避免

### 2. 求值策略

- **弱头范式 (WHNF)**：不继续求值函数体内的表达式
- **左结合**：`f x y` 求值为 `(f x) y`
- **正常顺序**：最左最外优先

### 3. 类型推断

- **统一算法**：解决类型约束
- **出现检查**：避免无限类型
- **泛化/实例化**：处理多态类型

## 扩展可能性

### 1. 添加更多类型

```haskell
-- 字符类型
'a' :: Char

-- 字符串类型
"hello" :: String

-- 列表类型
[1, 2, 3] :: [Int]
```

### 2. 添加更多控制结构

```haskell
-- 模式匹配
case x of
  0 -> 1
  n -> n * fact (n - 1)

-- Let 递归
let rec fact n = 
  if n == 0 then 1 else n * fact (n - 1)
in fact 5
```

### 3. 添加模块系统

```haskell
-- 模块定义
module Math where
  add = \\x -> \\y -> x + y
  mul = \\x -> \\y -> x * y

-- 模块导入
import Math
add 1 2
```

这个语法定义提供了完整的语言规范，可以作为实现和文档的双重参考。
# Phase 3: 解析器组合子 (Parser Combinators)

## 学习目标

- 理解什么是解析器组合子
- 掌握 Megaparsec 库的使用
- 实现完整的 lambda 演算词法/语法分析器
- 理解 Monad 在解析中的应用

---

## 3.1 什么是解析器?

### 3.1.1 解析器的本质

解析器是一个函数:

```haskell
type Parser a = String -> Either ParseError (a, String)
--              输入    ^       成功/失败        ^ 剩余输入
```

| 组件 | 含义 |
|------|------|
| `String` | 输入的字符流 |
| `a` | 解析结果的类型 (如 `Expr`) |
| `ParseError` | 解析失败时的错误信息 |
| `(a, String)` | 成功时返回结果和剩余输入 |

### 3.1.2 为什么叫"组合子"?

因为简单的解析器可以**组合**成复杂的解析器:

```haskell
-- 简单解析器
digit :: Parser Char    -- 解析一个数字
letter :: Parser Char   -- 解析一个字母

-- 组合成复杂解析器
number :: Parser Int    -- 解析整数 = 多个 digit 组合
identifier :: Parser String  -- 解析标识符 = letter + 多个 letter/digit
```

---

## 3.2 Megaparsec 入门

### 3.2.1 基本类型

```haskell
-- Megaparsec 的核心类型
type Parsec e s a = ...  -- 简化: Parsec e s a ≈ State s (Either e a)

-- 常用别名
type Parser = Parsec Void String  -- 无自定义错误,输入是 String
```

### 3.2.2 基础组合子

| 函数 | 类型 | 说明 | 示例 |
|------|------|------|------|
| `satisfy` | `(Char -> Bool) -> Parser Char` | 匹配满足条件的字符 | `satisfy isDigit` |
| `char` | `Char -> Parser Char` | 匹配特定字符 | `char 'a'` |
| `string` | `String -> Parser String` | 匹配特定字符串 | `string "lambda"` |
| `oneOf` | `String -> Parser Char` | 匹配字符串中的任一字符 | `oneOf "+-*"` |
| `noneOf` | `String -> Parser Char` | 匹配不在字符串中的字符 | `noneOf " \t\n"` |
| `space` | `Parser ()` | 匹配空白 | `space` |

### 3.2.3 Monad 组合

```haskell
-- 序列组合: do notation
lambda :: Parser Expr
lambda = do
  char '\\'                    -- 1. 解析 '\'
  space                        -- 2. 解析空白
  param <- identifier          -- 3. 解析参数名
  space                        -- 4. 解析空白
  string "->"                  -- 5. 解析 "->"
  body <- expr                 -- 6. 解析 body
  return $ Lam param body      -- 7. 返回 Lam 构造子

-- 相当于:
lambda = char '\\' >> space >> identifier >>= \param ->
         space >> string "->" >>
         expr >>= \body ->
         return (Lam param body)
```

### 3.2.4 Applicative 组合

```haskell
-- 使用 <$> 和 <*> 代替 do notation
-- <$> = fmap (函子映射)
-- <*> = 应用 ( applicative 应用)

parseAdd :: Parser Expr
parseAdd = Add <$> expr <* char '+' <*> expr
-- 相当于:
--   do e1 <- expr
--      char '+'
--      e2 <- expr
--      return (Add e1 e2)
```

### 3.2.5 选择与回溯

```haskell
-- <|> : 或运算符 (选择)
expr :: Parser Expr
expr = lambda <|> application <|> variable

-- 如果第一个解析器失败,尝试第二个
-- 注意: 需要回溯 (try)

expr :: Parser Expr
expr = (try lambda) <|> application <|> variable
--              ^^^
--              如果 lambda 消耗了输入但失败,回退到原位置
```

### 3.2.6 重复组合子

| 函数 | 类型 | 说明 | 示例 |
|------|------|------|------|
| `many` | `Parser a -> Parser [a]` | 匹配 0 次或多次 | `many digit` |
| `some` | `Parser a -> Parser [a]` | 匹配 1 次或多次 | `some letter` |
| `optional` | `Parser a -> Parser (Maybe a)` | 匹配 0 或 1 次 | `optional (char '+')` |
| `sepBy` | `Parser a -> Parser sep -> Parser [a]` | 分隔符重复 | `expr `sepBy` char ','` |
| `between` | `Parser open -> Parser close -> Parser a -> Parser a` | 匹配包围 | `parens = between (char '(') (char ')')` |

---

## 3.3 解析器架构

### 3.3.1 两阶段解析

```
原始字符串
    ↓
[词法分析 Lexer] → Token 流
    ↓
[语法分析 Parser] → AST
```

但对于 lambda 演算,我们可以**合并**为一个阶段:

```
原始字符串
    ↓
[解析器 Parser] → AST
```

### 3.3.2 处理空白

```haskell
-- 方式 1: 显式处理空白
lexeme :: Parser a -> Parser a
lexeme p = p <* space

-- 方式 2: 使用 Megaparsec 的内置功能
spaceConsumer :: Parser ()
spaceConsumer = L.space space1 lineComment blockComment
  where
    lineComment = L.skipLineComment "--"
    blockComment = L.skipBlockComment "{-" "-}"
```

---

## 3.4 实现 Lambda 演算解析器

### 3.4.1 标识符解析

```haskell
-- TODO: 实现 identifier 解析器
--
-- 标识符规则:
--   1. 首字符必须是字母或下划线
--   2. 后续字符可以是字母、数字、下划线
--   3. 不能是关键字 (lambda, if, then, else, true, false)
--
-- 提示:
--   使用 letter, char '_', alphaNum, many
--   使用 notFollowedBy 检查关键字

identifier :: Parser String
identifier = TODO
```

### 3.4.2 字面量解析

```haskell
-- TODO: 实现字面量解析器
--
-- 整数: 一串数字
-- 布尔: "true" 或 "false"

parseInt :: Parser Expr
parseInt = TODO
  -- 提示: 用 some digit 读取数字,然后转换为 Int

parseBool :: Parser Expr
parseBool = TODO
  -- 提示: 用 string "true" 或 "false",返回 LitBool
```

### 3.4.3 Lambda 表达式解析

```haskell
-- TODO: 实现 lambda 表达式解析器
--
-- 语法: '\' <identifier> '->' <expr>
-- 示例: \x -> x + 1
--
-- 提示:
--   1. 使用 char '\\' 解析反斜杠
--   2. 使用 lexeme 消费空白
--   3. 用 identifier 获取参数名
--   4. 解析 "->"
--   5. 解析 body 表达式

parseLambda :: Parser Expr
parseLambda = TODO
```

### 3.4.4 函数应用解析

```haskell
-- TODO: 实现函数应用解析器
--
-- 函数应用是左结合的: f x y = (f x) y
-- 语法: <atom>+ (一个或多个原子表达式)
-- 示例: f x, (\\x -> x) 5, add 1 2
--
-- 提示:
--   1. 先解析一个 atom (变量、字面量、括号表达式、lambda)
--   2. 用 some 解析后续的 atom
--   3. 将列表 [e1, e2, e3, ...] 转换为 App (App e1 e2) e3

parseApp :: Parser Expr
parseApp = TODO
```

### 3.4.5 原子表达式解析

```haskell
-- TODO: 实现原子表达式解析器
--
-- 原子表达式包括:
--   1. 变量: identifier
--   2. 字面量: parseInt, parseBool
--   3. 括号表达式: '(' expr ')'
--   4. Lambda 表达式 (可选,也可以在顶层处理)
--
-- 提示:
--   使用 <|> 组合多个解析器
--   使用 between 处理括号

parseAtom :: Parser Expr
parseAtom = TODO
```

### 3.4.6 运算符表达式解析

```haskell
-- TODO: 实现二元运算符解析器
--
-- 支持的运算符 (优先级从高到低):
--   1. * (乘法)
--   2. + (加法)
--   3. if-then-else (条件表达式)
--
-- 提示:
--   使用 makeExprParser 构建运算符解析器
--   或手动实现: 先解析高优先级,再解析低优先级

parseExpr :: Parser Expr
parseExpr = TODO
```

### 3.4.7 完整解析器

```haskell
-- TODO: 实现顶层解析器
--
-- 功能:
--   1. 消费前导空白
--   2. 解析表达式
--   3. 消费尾部空白
--   4. 确保没有剩余输入
--
-- 提示:
--   使用 between, spaceConsumer, eof

parseFullExpr :: String -> Either ParseError Expr
parseFullExpr input = parse (spaceConsumer *> parseExpr <* eof) "" input
```

---

## 3.5 代码骨架

打开 `src/Parser.hs` 并完成以下 TODO:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Parser
  ( parseExpr
  , parseFullExpr
  , parseTest
  ) where

import AST
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void (Void)
import Data.Functor (void)

-- 解析器类型
type Parser = Parsec Void String

-- ============================================
-- TODO 1: 实现空白处理
-- ============================================

-- 消费空白和注释
sc :: Parser ()
sc = L.space space1 lineComment blockComment
  where
    lineComment = TODO  -- TODO: 实现 "--" 单行注释
    blockComment = TODO  -- TODO: 实现 "{-" "-}" 块注释

-- 包装解析器,自动处理空白
lexeme :: Parser a -> Parser a
lexeme p = L.lexeme sc p

-- ============================================
-- TODO 2: 实现标识符解析
-- ============================================

-- 标识符: 字母开头,后跟字母数字或下划线
identifier :: Parser String
identifier = TODO

-- 关键字列表
keywords :: [String]
keywords = ["lambda", "if", "then", "else", "true", "false"]

-- ============================================
-- TODO 3: 实现字面量解析
-- ============================================

-- 解析整数
parseInt :: Parser Expr
parseInt = TODO

-- 解析布尔值
parseBool :: Parser Expr
parseBool = TODO

-- ============================================
-- TODO 4: 实现变量解析
-- ============================================

parseVar :: Parser Expr
parseVar = TODO

-- ============================================
-- TODO 5: 实现括号表达式
-- ============================================

parens :: Parser a -> Parser a
parens = TODO  -- 提示: between (lexeme (char '(')) (lexeme (char ')'))

-- ============================================
-- TODO 6: 实现原子表达式
-- ============================================

-- 原子表达式: 变量、字面量、括号表达式
parseAtom :: Parser Expr
parseAtom = TODO

-- ============================================
-- TODO 7: 实现 Lambda 表达式
-- ============================================

-- Lambda: \x -> body
parseLambda :: Parser Expr
parseLambda = TODO

-- ============================================
-- TODO 8: 实现函数应用
-- ============================================

-- 函数应用: 左结合
parseApp :: Parser Expr
parseApp = TODO

-- ============================================
-- TODO 9: 实现二元运算符
-- ============================================

-- 运算符定义 (使用 makeExprParser)
table :: [[Operator Parser Expr]]
table = TODO
  -- 提示:
  --   [ [ InfixL (Mul <$ lexeme (char '*')) ]
  --   , [ InfixL (Add <$ lexeme (char '+')) ]
  --   ]

-- ============================================
-- TODO 10: 实现条件表达式
-- ============================================

-- if c then t else f
parseIf :: Parser Expr
parseIf = TODO

-- ============================================
-- TODO 11: 实现顶层表达式解析器
-- ============================================

-- 表达式: if > 应用 > lambda > atom
parseExpr :: Parser Expr
parseExpr = TODO

-- 完整解析: 消费所有空白和输入
parseFullExpr :: String -> Either ParseError Bundle
parseFullExpr = TODO

-- ============================================
-- 测试函数
-- ============================================

-- 测试单个表达式
parseTest :: String -> IO ()
parseTest input = case parseFullExpr input of
  Left err -> putStrLn $ "Parse error: " ++ errorBundlePretty err
  Right expr -> putStrLn $ "Parsed: " ++ prettyPrintStr expr
```

---

## 3.6 测试用例

```haskell
-- 测试 1: 变量
parseTest "x"
-- 期望: Var "x"

-- 测试 2: 整数
parseTest "42"
-- 期望: LitInt 42

-- 测试 3: 布尔值
parseTest "true"
parseTest "false"
-- 期望: LitBool True, LitBool False

-- 测试 4: Lambda 表达式
parseTest "\\x -> x"
-- 期望: Lam "x" (Var "x")

parseTest "\\x -> x + 1"
-- 期望: Lam "x" (Add (Var "x") (LitInt 1))

-- 测试 5: 函数应用
parseTest "f x"
-- 期望: App (Var "f") (Var "x")

parseTest "(\\x -> x + 1) 5"
-- 期望: App (Lam "x" (Add (Var "x") (LitInt 1))) (LitInt 5)

-- 测试 6: 运算符优先级
parseTest "1 + 2 * 3"
-- 期望: Add (LitInt 1) (Mul (LitInt 2) (LitInt 3))

parseTest "1 * 2 + 3"
-- 期望: Add (Mul (LitInt 1) (LitInt 2)) (LitInt 3)

-- 测试 7: 条件表达式
parseTest "if true then 1 else 0"
-- 期望: If (LitBool True) (LitInt 1) (LitInt 0)

-- 测试 8: 复杂表达式
parseTest "\\f -> \\x -> f x"
-- 期望: Lam "f" (Lam "x" (App (Var "f") (Var "x")))

parseTest "(\\x -> x + 1) (\\y -> y * 2)"
-- 期望: App (Lam "x" (Add (Var "x") (LitInt 1))) (Lam "y" (Mul (Var "y") (LitInt 2)))
```

---

## 3.7 调试技巧

### 使用 `parseTest` 调试

```haskell
-- 在 GHCi 中:
import Parser
parseTest "\\x -> x"

-- 如果失败,会显示详细的错误信息
-- 例如:
-- Parse error at line 1, column 5:
--   unexpected '\\'
--   expecting "->"
```

### 使用 `dbg` 调试

```haskell
-- 在解析器中插入调试信息
parseAtom = dbg "parseAtom" $
  parseVar <|> parseInt <|> parseBool <|> parens parseExpr

-- 运行时会显示:
-- parseAtom: input "x", result Var "x"
```

---

## 3.8 扩展练习

### 挑战 1: 添加 Let 表达式

扩展解析器支持 `let` 表达式:

```
let x = 5 in x + 1
```

```haskell
parseLet :: Parser Expr
parseLet = do
  lexeme (string "let")
  x <- identifier
  lexeme (char '=')
  e1 <- parseExpr
  lexeme (string "in")
  e2 <- parseExpr
  return $ Let x e1 e2
```

### 挑战 2: 添加类型注解

扩展解析器支持类型注解:

```
(\x -> x) :: Int -> Int
```

```haskell
parseAnn :: Parser Expr
parseAnn = do
  e <- parseApp
  optional $ do
    lexeme (string "::")
    t <- parseType  -- 需要实现 parseType
    return $ Ann e t
  return e
```

---

## 下一步

完成解析器后,进入 **Phase 4: 类型推断**

你将学习:
- Hindley-Milner 类型推断算法
- 类型统一 (Unification)
- 类型环境 (Type Environment)
- 类型方案 (Type Scheme)

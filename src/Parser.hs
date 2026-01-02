{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Parser
-- Description : Parser combinators for Lambda Calculus
--
-- This module implements a lexer and parser for lambda calculus expressions
-- using the Megaparsec library.
--
-- Learning objectives:
--   * Understand parser combinators
--   * Learn to use Megaparsec
--   * Handle whitespace, comments, and operator precedence

module Parser
  ( -- * Parser type
    Parser
    -- * Parse functions
  , parseExpr
  , parseFullExpr
    -- * Test function
  , parseTest
  ) where

import AST
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void (Void)
import Data.Functor (void)
import Data.Char (isLetter, isAlphaNum)
import Control.Applicative ((<|>), many)
import Control.Monad (void)
import Data.Either (fromRight)

-- ============================================
-- Parser Type
-- ============================================

type Parser = Parsec Void String

-- ============================================
-- TODO 1: 实现空白和注释处理
-- ============================================

-- | 消费空白和注释
sc :: Parser ()
sc = TODO  -- 提示: L.space space1 lineComment blockComment
  where
    lineComment = TODO  -- TODO: 实现 "--" 单行注释
    blockComment = TODO  -- TODO: 实现 "{-" "-}" 块注释

-- | 包装解析器,自动处理周围的空白
lexeme :: Parser a -> Parser a
lexeme p = L.lexeme sc p

-- | 符号解析器 (不返回值)
symbol :: String -> Parser String
symbol = L.symbol sc

-- ============================================
-- TODO 2: 实现标识符解析
-- ============================================

-- | 标识符: 字母开头,后跟字母数字或下划线
identifier :: Parser String
identifier = lexeme $ do
  first <- letterChar
  rest <- many (alphaNumChar <|> char '_')
  let name = first : rest
  if name `elem` keywords
    then fail $ "Reserved keyword: " ++ name
    else return name

-- | 关键字列表
keywords :: [String]
keywords = ["lambda", "if", "then", "else", "true", "false", "let", "in", "rec"]

-- ============================================
-- TODO 3: 实现字面量解析
-- ============================================

-- | 解析整数
parseInt :: Parser Expr
parseInt = TODO
  -- 提示:
  --   1. 用 L.signed spaceConsumer (L.decimal) 解析有符号整数
  --   2. 返回 LitInt

-- | 解析布尔值
parseBool :: Parser Expr
parseBool = TODO
  -- 提示:
  --   用 (string "true" $> LitBool True) <|> (string "false" $> LitBool False)
  --   $> 是 fmap (const ...) 的简写

-- ============================================
-- TODO 4: 实现变量解析
-- ============================================

parseVar :: Parser Expr
parseVar = TODO  -- 提示: Var <$> identifier

-- ============================================
-- TODO 5: 实现括号表达式
-- ============================================

-- | 括号包裹的表达式
parens :: Parser a -> Parser a
parens = TODO  -- 提示: between (symbol "(") (symbol ")")

-- ============================================
-- TODO 6: 实现原子表达式
-- ============================================

-- | 原子表达式: 变量、字面量、括号表达式
parseAtom :: Parser Expr
parseAtom = TODO
  -- 提示:
  --   用 <|> 组合:
  --     parseInt
  --     parseBool
  --     parseVar
  --     parens parseExpr

-- ============================================
-- TODO 7: 实现 Lambda 表达式
-- ============================================

-- | Lambda: \x -> body
parseLambda :: Parser Expr
parseLambda = TODO
  -- 提示:
  --   1. 解析 '\' 或 '\\'
  --   2. 解析参数名 identifier
  --   3. 解析 "->"
  --   4. 解析 body
  --   5. 返回 Lam param body

-- ============================================
-- TODO 8: 实现函数应用 (左结合)
-- ============================================

-- | 函数应用: f x y z = ((f x) y) z
parseApp :: Parser Expr
parseApp = TODO
  -- 提示:
  --   1. 用 some 解析一个或多个 parseAtom
  --   2. 将列表 [e1, e2, e3] 转换为 App (App e1 e2) e3
  --   3. 用 foldl1 实现左结合

-- ============================================
-- TODO 9: 实现条件表达式
-- ============================================

-- | if c then t else f
parseIf :: Parser Expr
parseIf = TODO
  -- 提示:
  --   1. 解析 "if"
  --   2. 解析条件 c
  --   3. 解析 "then"
  --   4. 解析 true 分支 t
  --   5. 解析 "else"
  --   6. 解析 false 分支 e
  --   7. 返回 If c t e

-- ============================================
-- TODO 10: 实现运算符表达式
-- ============================================

-- | 使用 makeExprParser 构建运算符表达式
--
-- 优先级 (从高到低):
--   1. * (乘法)
--   2. + (加法)
--   3. if-then-else (条件)
--   4. -> (Lambda)
--   5. 空格 (函数应用)
parseExpr :: Parser Expr
parseExpr = TODO
  -- 提示:
  --   使用 makeExprParser parseOperator table
  --   或手动实现: parseIf <|> parseLambda <|> parseChain

-- | 运算符表
table :: [[Operator Parser Expr]]
table = TODO
  -- 提示:
  --   [ [ InfixL (Mul <$ symbol "*") ]
  --   , [ InfixL (Add <$ symbol "+") ]
  --   ]

-- ============================================
-- TODO 11: 实现顶层解析器
-- ============================================

-- | 完整解析: 消费所有空白和输入
parseFullExpr :: String -> Either ParseErrorBundle String Void Expr
parseFullExpr input = parse (spaceConsumer *> parseExpr <* eof) "" input

-- ============================================
-- 测试函数
-- ============================================

-- | 测试单个表达式 (用于调试)
parseTest :: String -> IO ()
parseTest input = case parseFullExpr input of
  Left err -> putStrLn $ "Parse error:\n" ++ errorBundlePretty err
  Right expr -> putStrLn $ "Parsed: " ++ prettyPrintStr expr

-- ============================================
-- 辅助函数
-- ============================================

-- | 从 AST 导入 prettyPrintStr
-- (这个函数在 AST.hs 中实现)
import qualified AST

prettyPrintStr :: Expr -> String
prettyPrintStr = AST.prettyPrintStr

-- ============================================
-- 调试辅助
-- ============================================

-- | 在解析器中插入调试信息
-- parseAtom = dbg "parseAtom" $
--   parseVar <|> parseInt <|> parseBool <|> parens parseExpr

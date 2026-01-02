{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Main (Test Suite)
-- Description : Test suite for Lambda Interpreter
--
-- This module contains the test suite using Tasty and QuickCheck.
--
-- Learning objectives:
--   * Write unit tests with HUnit-style assertions
--   * Write property tests with QuickCheck
--   * Understand test-driven development

module Main where

import Test.Tasty
import Test.Tasty.HUnit as HU
import Test.Tasty.QuickCheck as QC
import AST
import TypeChecker
import Evaluator
import Parser
import Data.Either (isRight, fromRight)
import Text.Megaparsec (errorBundlePretty)

-- ============================================
-- Main Test Runner
-- ============================================

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Lambda Interpreter Tests"
  [ parserTests
  , typeCheckerTests
  , evaluatorTests
  , integrationTests
  , propertyTests
  ]

-- ============================================
-- TODO 1: 解析器测试
-- ============================================

parserTests :: TestTree
parserTests = testGroup "Parser"
  [ HU.testCase "Parse variable" $
      parseFullExpr "x" @?= Right (Var "x")

  , HU.testCase "Parse number" $
      parseFullExpr "42" @?= Right (LitInt 42)

  , HU.testCase "Parse boolean true" $
      parseFullExpr "true" @?= Right (LitBool True)

  , HU.testCase "Parse boolean false" $
      parseFullExpr "false" @?= Right (LitBool False)

  , HU.testCase "Parse lambda" $
      parseFullExpr "\\x -> x" @?= Right (Lam "x" (Var "x"))

  , HU.testCase "Parse application" $
      parseFullExpr "f x" @?= Right (App (Var "f") (Var "x"))

  , HU.testCase "Parse addition" $
      parseFullExpr "1 + 2" @?= Right (Add (LitInt 1) (LitInt 2))

  , HU.testCase "Parse multiplication" $
      parseFullExpr "2 * 3" @?= Right (Mul (LitInt 2) (LitInt 3))

  , HU.testCase "Parse operator precedence" $
      parseFullExpr "1 + 2 * 3" @?= Right (Add (LitInt 1) (Mul (LitInt 2) (LitInt 3)))

  , HU.testCase "Parse parenthesized expression" $
      parseFullExpr "(1 + 2) * 3" @?= Right (Mul (Add (LitInt 1) (LitInt 2)) (LitInt 3))

  , HU.testCase "Parse if-then-else" $
      parseFullExpr "if true then 1 else 0" @?= Right (If (LitBool True) (LitInt 1) (LitInt 0))

  -- TODO: 添加更多测试用例
  --   - 嵌套 Lambda: \\f -> \\x -> f x
  --   - 复杂应用: (\\x -> x + 1) 5
  --   - Y 组合子
  ]

-- ============================================
-- TODO 2: 类型检查器测试
-- ============================================

typeCheckerTests :: TestTree
typeCheckerTests = testGroup "Type Checker"
  [ HU.testCase "Type of Int literal" $
      TypeChecker.infer TypeChecker.emptyEnv (LitInt 42)
        @?= Right TInt

  , HU.testCase "Type of Bool literal" $
      TypeChecker.infer TypeChecker.emptyEnv (LitBool True)
        @?= Right TBool

  , HU.testCase "Type of identity lambda" $
      case TypeChecker.infer TypeChecker.emptyEnv (Lam "x" (Var "x")) of
        Right (TFun t1 t2) -> t1 @?= t2
        _ -> HU.assertFailure "Expected function type"

  , HU.testCase "Type of addition" $
      TypeChecker.infer TypeChecker.emptyEnv (Add (LitInt 1) (LitInt 2))
        @?= Right TInt

  , HU.testCase "Type of application" $
      TypeChecker.infer TypeChecker.emptyEnv (App (Lam "x" (Var "x")) (LitInt 5))
        @?= Right TInt

  , HU.testCase "Type error: unbound variable" $
      case TypeChecker.infer TypeChecker.emptyEnv (Var "undefined_var") of
        Left (UnboundVariable _) -> HU.assertBool "" True
        _ -> HU.assertFailure "Expected UnboundVariable error"

  , HU.testCase "Type error: cannot unify Int and Bool" $
      case TypeChecker.infer TypeChecker.emptyEnv (Add (LitInt 1) (LitBool True)) of
        Left (UnificationError TInt TBool) -> HU.assertBool "" True
        _ -> HU.assertFailure "Expected UnificationError"

  -- TODO: 添加更多测试用例
  --   - 高阶函数类型
  --   - 类型错误: if 条件不是 Bool
  --   - 多态类型
  ]

-- ============================================
-- TODO 3: 求值器测试
-- ============================================

evaluatorTests :: TestTree
evaluatorTests = testGroup "Evaluator"
  [ HU.testCase "Eval Int literal" $
      eval (LitInt 42) @?= LitInt 42

  , HU.testCase "Eval Bool literal" $
      eval (LitBool True) @?= (LitBool True)

  , HU.testCase "Eval addition" $
      eval (Add (LitInt 1) (LitInt 2)) @?= LitInt 3

  , HU.testCase "Eval multiplication" $
      eval (Mul (LitInt 3) (LitInt 4)) @?= LitInt 12

  , HU.testCase "Eval beta reduction" $
      eval (App (Lam "x" (Var "x")) (LitInt 5)) @?= LitInt 5

  , HU.testCase "Eval if-then-else (true branch)" $
      eval (If (LitBool True) (LitInt 1) (LitInt 0)) @?= LitInt 1

  , HU.testCase "Eval if-then-else (false branch)" $
      eval (If (LitBool False) (LitInt 1) (LitInt 0)) @?= LitInt 0

  , HU.testCase "Eval nested application" $
      eval (App (App (Lam "f" (Lam "x" (App (Var "f") (Var "x"))))
                    (Lam "y" (Add (Var "y") (LitInt 1)))
                ) (LitInt 5))
      @?= LitInt 6

  -- TODO: 添加更多测试用例
  --   - Church 编码 (如果实现了)
  --   - Y 组合子
  --   - 递归函数
  ]

-- ============================================
-- TODO 4: 集成测试
-- ============================================

integrationTests :: TestTree
integrationTests = testGroup "Integration"
  [ HU.testCase "Parse + TypeCheck + Eval: identity" $ do
      let input = "(\\x -> x) 42"
      case parseFullExpr input of
        Left err -> HU.assertFailure $ "Parse failed: " ++ errorBundlePretty err
        Right expr -> do
          -- Type check
          case TypeChecker.infer TypeChecker.emptyEnv expr of
            Left _ -> HU.assertFailure "Type check failed"
            Right t -> t @?= TInt
            -- Eval
            eval expr @?= LitInt 42

  , HU.testCase "Parse + TypeCheck + Eval: arithmetic" $ do
      let input = "(\\x -> x + 1) 5"
      case parseFullExpr input of
        Left err -> HU.assertFailure $ "Parse failed: " ++ errorBundlePretty err
        Right expr -> do
          case TypeChecker.infer TypeChecker.emptyEnv expr of
            Left _ -> HU.assertFailure "Type check failed"
            Right _ -> eval expr @?= LitInt 6

  , HU.testCase "Parse + TypeCheck + Eval: higher order" $ do
      let input = "(\\f -> \\x -> f x) (\\y -> y + 1) 10"
      case parseFullExpr input of
        Left err -> HU.assertFailure $ "Parse failed: " ++ errorBundlePretty err
        Right expr -> do
          case TypeChecker.infer TypeChecker.emptyEnv expr of
            Left _ -> HU.assertFailure "Type check failed"
            Right _ -> eval expr @?= LitInt 11

  -- TODO: 添加更多集成测试
  ]

-- ============================================
-- TODO 5: 属性测试
-- ============================================

propertyTests :: TestTree
propertyTests = testGroup "Properties"
  [ QC.testProperty "Type preservation: eval preserves type" prop_typePreservation
  , QC.testProperty "Value idempotent: values are unchanged by eval" prop_valueIdempotent
  , QC.testProperty "Add is commutative" prop_addCommutative
  , QC.testProperty "Add is associative" prop_addAssociative
  ]

-- | TODO: 属性 1 - 类型保持
-- 求值不应该改变表达式的类型
prop_typePreservation :: Expr -> QC.Property
prop_typePreservation expr = QC.property $
  case TypeChecker.infer TypeChecker.emptyEnv expr of
    Left _ -> QC.discard  -- 跳过类型错误的表达式
    Right t ->
      case TypeChecker.infer TypeChecker.emptyEnv (eval expr) of
        Left _ -> QC.counterexample "Type changed after evaluation" False
        Right t' -> t == t'

-- | TODO: 属性 2 - 值的幂等性
-- 如果 e 已经是值,eval e == e
prop_valueIdempotent :: Expr -> QC.Property
prop_valueIdempotent expr = TODO
  -- 提示:
  --   QC.implies (isValue expr) (eval expr == expr)

-- | TODO: 属性 3 - 加法交换律
-- a + b == b + a (对于整数)
prop_addCommutative :: Expr -> Expr -> QC.Property
prop_addCommutative e1 e2 = TODO
  -- 提示:
  --   1. 检查 e1 和 e2 是否都是整数
  --   2. 比较 eval (Add e1 e2) 和 eval (Add e2 e1)

-- | TODO: 属性 4 - 加法结合律
-- (a + b) + c == a + (b + c)
prop_addAssociative :: Expr -> Expr -> Expr -> QC.Property
prop_addAssociative a b c = TODO

-- ============================================
-- 辅助: 为 Expr 实现 Arbitrary
-- ============================================

-- TODO: 实现 Arbitrary Expr 实例
-- 用于生成随机表达式进行属性测试

instance Arbitrary Expr where
  arbitrary = TODO
  -- 提示:
  --   使用 sized 控制生成大小
  --   使用 frequency 控制生成分布
  --
  --   示例:
  --     sized $ \n ->
  --       frequency
  --         [ (3, LitInt <$> arbitrary)
  --         , (2, Lam <$> elements ["x","y","z"] <*> resize (n `div` 2) arbitrary)
  --         , (2, App <$> resize (n `div` 2) arbitrary <*> resize (n `div` 2) arbitrary)
  --         , (1, Add <$> resize (n `div` 2) arbitrary <*> resize (n `div` 2) arbitrary)
  --         ]

  shrink = TODO  -- 可选: 实现缩小逻辑

-- ============================================
-- 导入必要的模块
-- ============================================

import Test.Tasty.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Data.Maybe (isJust, fromJust)
import qualified Data.Set as Set

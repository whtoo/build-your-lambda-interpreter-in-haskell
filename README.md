# Lambda Calculus Interpreter - Haskell 学习项目

> 📖 **语言语法**: [查看完整的语法定义与例子](docs/language_syntax.md)

这是一个**项目制 Haskell 教程**,通过从零构建一个类型安全的 Lambda 演算解释器,深入学习 Haskell 的核心概念。

## 项目概述

你将构建一个支持以下功能的 Lambda 演算解释器:
- ✅ Lambda 表达式 (抽象、应用、变量)
- ✅ 基本类型 (Int, Bool)
- ✅ 算术运算 (+, *)
- ✅ 条件表达式 (if-then-else)
- ✅ Hindley-Milner 类型推断
- ✅ REPL (交互式解释器)

## 学习路径

### 课程结构

```
docs/
├── language_syntax.md           # 📚 语言语法定义与例子
├── 01_environment_setup.md      # Day 1: 环境配置
├── 02_adt_and_ast.md            # Day 2-3: ADT 与 AST
├── 03_parser_combinators.md     # Day 4-5: 解析器组合子
├── 04_type_inference.md         # Day 6-8: 类型推断
├── 05_evaluation.md             # Day 9-10: 求值器
└── 06_repl_and_testing.md       # Day 11-12: REPL 与测试
```

### 代码结构

```
haskell_divein/
├── docs/                    # 📚 课程文档 (从这里开始!)
├── src/
│   ├── AST.hs              # ✅ 骨架完成 - 等你填写 TODO
│   ├── Parser.hs           # ✅ 骨架完成 - 等你填写 TODO
│   ├── TypeChecker.hs      # ✅ 骨架完成 - 等你填写 TODO
│   ├── Evaluator.hs        # ✅ 骨架完成 - 等你填写 TODO
│   └── REPL.hs             # ✅ 骨架完成 - 等你填写 TODO
├── app/
│   └── Main.hs             # ✅ 入口点
├── test/
│   └── Spec.hs             # ✅ 测试套件
├── package.yaml            # 项目配置
└── stack.yaml              # Stack 配置
```

---

## 快速开始

### 1. 安装 Stack

```bash
# macOS (推荐)
brew install ghcup
ghcup install stack

# Linux
curl -sSL https://get.haskellstack.org/ | sh

# 验证安装
stack --version
```

### 2. 编译项目

```bash
cd haskell_divein
stack build
```

### 3. 运行测试

```bash
stack test
```

### 4. 启动 REPL

```bash
stack run
```

---

## 学习指南

### 第一步: 阅读文档

每个 Phase 包含:
1. **概念讲解**: 理论背景和原理
2. **代码示例**: 展示如何使用
3. **TODO 标记**: 你需要实现的代码位置
4. **测试用例**: 验证你的实现

### 第二步: 实现代码

在对应的 `.hs` 文件中找到 `TODO` 标记:

```haskell
-- TODO: 实现这个函数
myFunction :: Type -> Type
myFunction = TODO  -- 替换为你的实现
```

### 第三步: 编译测试

```bash
# 编译
stack build

# 运行测试
stack test

# 启动 GHCi 调试
stack ghci
```

### 第四步: 验证结果

```haskell
-- 在 GHCi 中:
:load src/AST.hs
-- 测试你的函数
size (Add (LitInt 1) (LitInt 2))
-- 期望: 3
```

---

## 学习进度检查表

### Phase 1: 环境配置 ✅

- [ ] Stack 安装成功
- [ ] `stack build` 编译通过
- [ ] `stack test` 运行测试
- [ ] `stack ghci` 进入交互式环境

**文件**: `docs/01_environment_setup.md`

---

### Phase 2: ADT 与 AST 🔄

- [ ] 理解代数数据类型 (ADT)
- [ ] 定义 `Expr` 类型
- [ ] 定义 `Type` 类型
- [ ] 实现 `prettyPrint` 函数
- [ ] 实现 `size` 函数
- [ ] 实现 `freeVars` 函数

**文件**: `src/AST.hs` | **文档**: `docs/02_adt_and_ast.md`

---

### Phase 3: 解析器组合子 🔄

- [ ] 理解 Megaparsec 基础
- [ ] 实现空白处理 `sc`
- [ ] 实现标识符解析 `identifier`
- [ ] 实现字面量解析 `parseInt`, `parseBool`
- [ ] 实现 Lambda 解析 `parseLambda`
- [ ] 实现运算符解析 `parseExpr`

**文件**: `src/Parser.hs` | **文档**: `docs/03_parser_combinators.md`

---

### Phase 4: 类型推断 🔄

- [ ] 理解类型替换 `apply`
- [ ] 理解类型统一 `unify`
- [ ] 实现 occurs 检查
- [ ] 实现 `infer` 函数
- [ ] 处理各种表达式类型

**文件**: `src/TypeChecker.hs` | **文档**: `docs/04_type_inference.md`

---

### Phase 5: 求值器 🔄

- [ ] 理解 WHNF vs NF
- [ ] 实现 `substitute` 函数
- [ ] 实现 `step` (单步求值)
- [ ] 实现 `eval` (大步求值)
- [ ] 理解 Beta 归约

**文件**: `src/Evaluator.hs` | **文档**: `docs/05_evaluation.md`

---

### Phase 6: REPL 与测试 🔄

- [ ] 理解 Monad Transformer
- [ ] 实现 `parseCommand`
- [ ] 实现 `executeCommand`
- [ ] 实现 `repl` 主循环
- [ ] 编写测试用例

**文件**: `src/REPL.hs`, `test/Spec.hs` | **文档**: `docs/06_repl_and_testing.md`

---

## 示例会话

```haskell
-- 启动 REPL
stack run

-- 定义恒等函数
λ0> let id = \x -> x
Defined id : t0 -> t0

-- 应用函数
λ1> id 42
42 : Int

-- Lambda 表达式
λ2> \x -> x + 1
\x -> x + 1 : Int -> Int

-- 函数应用
λ3> (\x -> x + 1) 5
6 : Int

-- 高阶函数
λ4> let apply = \f -> \x -> f x
Defined apply : (t1 -> t2) -> t1 -> t2

λ5> apply (\x -> x * 2) 10
20 : Int

-- 查看类型
λ6> :t apply
apply : (t1 -> t2) -> t1 -> t2

-- 历史记录
λ7> :history
0: let id = \x -> x
1: id 42
2: \x -> x + 1
...

-- 退出
λ8> :quit
```

---

## 常见问题

### Q: 编译错误 "Could not find module"

**A**: 确保在项目根目录运行 `stack build`,Stack 会自动管理依赖。

### Q: 如何调试解析器?

**A**: 使用 `parseTest` 函数:

```haskell
stack ghci
> :load src/Parser.hs
> parseTest "\\x -> x"
```

### Q: 类型推断失败怎么办?

**A**:
1. 检查表达式是否语法正确
2. 使用 `:t` 查看表达式的类型
3. 查看错误信息,理解哪个地方类型不匹配

### Q: 测试失败怎么调试?

**A**:
```bash
# 运行特定测试
stack test --test-arguments="--parser-tests"

# 带详细输出
stack test --test-arguments="-v"
```

---

## 扩展项目

完成基础后,可以尝试:

1. **添加 Let 表达式**
   ```haskell
   let x = 5 in x + 1
   ```

2. **添加递归支持**
   ```haskell
   let rec fact = \n ->
     if n == 0 then 1 else n * fact (n - 1)
   ```

3. **添加代数数据类型**
   ```haskell
   data List a = Nil | Cons a (List a)
   ```

4. **编译到字节码**
   - 实现栈机器
   - 或编译到 JVM

5. **优化**
   - 闭包转换
   - Hoisting
   - 内联优化

---

## 推荐资源

### 书籍

1. **"Haskell Programming from First Principles"** - Christopher Allen
2. **"Type Theory and Functional Programming"** - Simon Thompson
3. **"Parallel and Concurrent Programming in Haskell"** - Simon Marlow

### 在线资源

- [Haskell Wiki](https://wiki.haskell.org/)
- [Learn Haskell](https://learnyouahaskell.github.io/)
- [Haskell MOOC](https://haskell.mooc.fi/)

### 开源项目

- [GHC](https://gitlab.haskell.org/ghc/ghc) - Glasgow Haskell Compiler
- [Pandoc](https://github.com/jgm/pandoc) - 文档转换器
- [XMonad](https://github.com/xmonad/xmonad) - 窗口管理器

---

## 贡献

欢迎提交 Issue 和 Pull Request!

## 许可证

MIT License

---

**祝你学习愉快!** 🚀

如有问题,随时告诉我!

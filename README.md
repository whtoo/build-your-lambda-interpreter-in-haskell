# Lambda Calculus Interpreter - Haskell 学习项目

> 📖 **语言语法**: [查看完整的语法定义与例子](docs/language_syntax.md)

## 🎯 项目目的

这是一个**项目制 Haskell 教程**，通过从零构建一个**类型安全的 Lambda 演算解释器**，深入学习 Haskell 的核心概念：

- **类型系统**: 代数数据类型 (ADT)、模式匹配、类型推断
- **函数式编程**: 高阶函数、Lambda 演算、不可变数据
- **解析技术**: 解析器组合子、词法分析、语法分析  
- **理论计算机科学**: Hindley-Milner 类型推断、Beta 归约、弱头范式

## 🚀 快速开始 (30 秒上手)

### 1. 环境要求
```bash
# 安装 Haskell Stack (如果尚未安装)
curl -sSL https://get.haskellstack.org/ | sh
```

### 2. 克隆并构建
```bash
git clone <your-repo-url> haskell_divein
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

### 5. 验证安装
```haskell
-- 在 REPL 中输入:
\x -> x + 1
-- 应该显示: \x -> x + 1 : Int -> Int
```

## ⚡ 快速参考

### 开发命令速查
```bash
# 环境管理
stack build              # 编译项目
stack test               # 运行所有测试
stack run                # 启动 REPL
stack ghci               # 进入交互式开发环境

# 调试工具
stack ghci               # 加载项目到 GHCi
:load src/AST.hs         # 加载特定模块
:reload                  # 重新加载修改后的代码

# 项目维护  
stack clean              # 清理构建缓存
stack update             # 更新 Stack 索引
```

### REPL 命令速查
```haskell
-- 基本用法
\x -> x + 1              -- 定义 Lambda 表达式
let id = \x -> x         -- 定义命名函数
id 42                    -- 函数应用

-- 类型查询
:t id                    -- 查看表达式类型
:history                 -- 查看历史记录
:quit                    -- 退出 REPL

-- 调试帮助
:load src/Parser.hs      -- 加载模块
parseTest "1 + 2 * 3"    -- 测试解析器
```

## 🎮 语言特性预览

我们的解释器支持丰富的函数式编程特性：

### 基本表达式
```haskell
42                      -- 整数字面量
true                    -- 布尔字面量
\x -> x + 1             -- Lambda 表达式 (匿名函数)
```

### 高阶函数
```haskell
let apply = \f -> \x -> f x
apply (\x -> x * 2) 10  -- = 20

let compose = \f -> \g -> \x -> f (g x)
compose (\x -> x + 1) (\x -> x * 2) 5  -- = 11
```

### 条件表达式  
```haskell
if true then 1 else 0                    -- = 1
if 1 + 1 == 2 then "yes" else "no"       -- = "yes"
\x -> if x > 0 then x else 0             -- 正数截断函数
```

### 复杂类型推断
```haskell
-- 以下表达式的类型会被自动推断为:
\f -> \g -> \x -> f (g x)  -- ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c
\x -> x                    -- 'a -> 'a (多态恒等函数)
\x -> \y -> x              -- 'a -> 'b -> 'a (常数函数)
```

### 实际可用的例子
```haskell
-- 阶乘函数 (使用 Y 组合子)
let Y = \f -> (\x -> f (x x)) (\x -> f (x x)) in
let fact = Y (\f -> \n -> if n == 0 then 1 else n * f (n - 1)) in
fact 5  -- = 120

-- 列表操作 (Church 编码)
let cons = \x -> \xs -> \c -> c x xs in
let nil = \c -> c in
let head = \xs -> xs (\x -> \xs -> x) in
let list = cons 1 (cons 2 (cons 3 nil)) in
head list  -- = 1
```

## 🔧 故障排除

### 常见问题

#### ❌ `stack build` 失败
```bash
# 问题: 依赖下载失败或编译错误
# 解决:
stack clean
stack build --only-dependencies  # 只安装依赖
stack build
```

#### ❌ `stack test` 失败
```bash
# 问题: 测试用例失败
# 解决: 查看具体失败信息
stack test --test-arguments="-v"  # 详细输出
stack test --test-arguments="--pattern=AST"  # 只测试 AST 模块
```

#### ❌ `stack run` 启动失败
```bash
# 问题: REPL 无法启动
# 解决: 检查编译状态
stack build  # 确保项目已编译
stack exec lambda-interpreter-exe  # 直接运行可执行文件
```

#### ❌ GHCi 加载模块失败
```haskell
-- 问题: 模块找不到
-- 解决: 在项目根目录启动 GHCi
stack ghci
-- 在 GHCi 中:
:set prompt "λ> "  -- 设置漂亮的提示符
```

### 环境配置检查

```bash
# 验证 Haskell 工具链
stack --version        # Stack 版本
gcc --version          # C 编译器 (macOS/Linux)
which make             # Make 工具

# 项目状态检查
git status             # 查看项目状态
stack exec which ghc   # 查看使用的 GHC 版本
```

### 性能优化

```bash
# 加快编译速度
stack build --fast     # 跳过优化，加快开发编译
stack build --profile  # 启用性能分析

# 清理和重建
stack clean
stack build
```

## 📖 完整文档

### 🏗️ 项目架构

```
lambda-interpreter/
├── app/
│   └── Main.hs              # 程序入口点
├── src/
│   ├── AST.hs               # 抽象语法树定义
│   ├── Parser.hs            # 解析器实现
│   ├── TypeChecker.hs       # 类型推断器
│   ├── Evaluator.hs         # 表达式求值器
│   └── REPL.hs              # 交互式环境
├── test/
│   └── Spec.hs              # 测试套件
├── docs/                    # 完整教程文档
└── package.yaml             # 项目配置
```

### 📚 学习路径 (建议 12 天完成)

| 阶段 | 时间 | 主题 | 技能要点 |
|------|------|------|----------|
| 1 | Day 1 | [环境配置](docs/01_environment_setup.md) | Stack 工具链、GHCi 使用 |
| 2 | Day 2-3 | [ADT 与 AST](docs/02_adt_and_ast.md) | 代数数据类型、模式匹配 |
| 3 | Day 4-5 | [解析器组合子](docs/03_parser_combinators.md) | Megaparsec、语法分析 |
| 4 | Day 6-8 | [类型推断](docs/04_type_inference.md) | Hindley-Milner 算法 |
| 5 | Day 9-10 | [求值器](docs/05_evaluation.md) | Beta 归约、求值策略 |
| 6 | Day 11-12 | [REPL 与测试](docs/06_repl_and_testing.md) | Monad 变换器、测试框架 |

### 🎯 学习目标

#### 核心概念
- ✅ **Lambda 演算**: 变量、抽象、应用
- ✅ **基本类型**: Int、Bool 和类型推断
- ✅ **算术运算**: +、* 运算符和优先级
- ✅ **条件表达式**: if-then-else
- ✅ **高阶函数**: 函数作为一等公民

#### 高级主题
- ✅ **类型系统**: Hindley-Milner 类型推断算法
- ✅ **解析技术**: Megaparsec 解析器组合子
- ✅ **求值策略**: 弱头范式、Beta 归约
- ✅ **Monad 变换器**: REPL 状态管理

## 💻 开发环境

### 系统要求
- **操作系统**: macOS、Linux、Windows (WSL 推荐)
- **Haskell Stack**: 最新版本
- **内存**: 4GB+ (推荐 8GB)
- **存储**: 2GB 可用空间

### 常用命令
```bash
stack build          # 编译项目
stack test           # 运行测试套件  
stack run            # 启动 REPL
stack ghci           # 进入交互式开发环境
stack clean          # 清理构建缓存
```

## 📋 学习进度检查表

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

## 🎮 示例会话

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

## 🔧 常见问题

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

## 🚀 扩展项目

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

## 📚 推荐资源

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

## 🤝 贡献

欢迎提交 Issue 和 Pull Request!

### 贡献指南
1. Fork 项目
2. 创建特性分支 (`git checkout -b feature/amazing-feature`)
3. 提交更改 (`git commit -m 'Add amazing feature'`)
4. 推送到分支 (`git push origin feature/amazing-feature`)
5. 创建 Pull Request

---

## 📄 许可证

MIT License - 详见 [LICENSE](LICENSE) 文件

---

**祝你学习愉快!** 🚀  
如有问题,随时告诉我!
# Phase 1: 环境配置与工具链

## 学习目标

- 安装 Haskell 工具链 (Stack)
- 理解项目结构
- 配置开发环境

---

## 1.1 Haskell 工具链简介

### 为什么需要 Stack?

Haskell 有两个主流构建工具：
- **Cabal**: 最早期的构建工具,功能强大但配置较复杂
- **Stack**: (推荐) 零配置,自动管理依赖版本,适合学习和生产

我们使用 Stack,因为它:
- 自动处理 GHC 版本和依赖
- 隔离项目环境,避免"依赖地狱"
- 内置项目模板和测试工具

### 安装 Stack

#### 方法 1: 使用 GHCup (推荐,跨平台)

```bash
# GHCup 是 GHC 的官方安装管理器
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

安装过程中会询问:
- 是否安装 GHC (选择 Yes,推荐 9.4.x)
- 是否安装 Cabal (选择 Yes)
- 是否安装 Stack (选择 Yes)
- 是否安装 HLS (选择 Yes,这是智能提示工具)

#### 方法 2: macOS Homebrew

```bash
brew install ghcup   # 先安装 ghcup
ghcup install stack  # 再通过 ghcup 安装 stack
```

#### 验证安装

```bash
stack --version
# 应输出: Version 2.x.x, Git revision xxx

# 查看 GHC 版本
stack ghc -- --version
# 应输出: The Glorious Glasgow Haskell Compilation System, version 9.4.x
```

---

## 1.2 项目结构解析

```
haskell_divein/
├── stack.yaml              # Stack 配置文件
├── package.yaml            # 包定义 (依赖、模块列表)
├── docs/                   # 课程文档 (你现在读的)
├── src/                    # 库源码
│   ├── AST.hs             # 抽象语法树定义
│   ├── Parser.hs          # 词法/语法分析器
│   ├── TypeChecker.hs     # 类型推断引擎
│   ├── Evaluator.hs       # 求值器
│   └── REPL.hs            # 交互式解释器
├── app/                    # 可执行程序
│   └── Main.hs            # 入口点
├── test/                   # 测试代码
│   └── Spec.hs            # QuickCheck 属性测试
└── .stack-work/           # Stack 生成的文件 (不要修改)
```

### 关键文件说明

#### `stack.yaml` - Stack 配置

```yaml
resolver: lts-21.25   # LTS Haskell 21.25,指定 GHC 9.4.x
packages:
  - .                # 当前目录作为包
```

**学习点:**
- `resolver` 决定了 GHC 版本和所有依赖的版本
- LTS (Long Term Support) 版本是稳定的快照
- 保持 resolver 一致可以确保团队环境一致

#### `package.yaml` - 包定义

```yaml
name: lambda-interpreter
dependencies:
  - base >= 4.17 && < 5      # 基础库
  - megaparsec >= 9.0         # 解析器组合子
  - prettyprinter >= 1.7      # 美化输出
  - containers >= 0.6         # Map, Data.Set 等
  - mtl >= 2.3                # Monad Transformer Library

library:
  source-dirs: src            # 库代码位置

executables:
  lambda-interpreter-exe:
    main: Main.hs
    source-dirs: app
    dependencies:
      - lambda-interpreter     # 依赖自己的库

tests:
  lambda-interpreter-test:
    main: Spec.hs
    source-dirs: test
```

**学习点:**
- `dependencies` 列出所有需要的包
- `library` 定义可复用的库代码
- `executables` 定义可执行程序
- `tests` 定义测试套件

---

## 1.3 常用 Stack 命令

### 项目初始化

```bash
# 在项目目录下
stack setup          # 首次运行,下载 GHC
stack build          # 编译项目
stack test           # 运行测试
stack run            # 运行可执行程序
```

### 开发循环

```bash
# 1. 修改代码后
stack build

# 2. 如果想看编译错误详情
stack build --ghc-options -fno-code

# 3. 运行测试
stack test

# 4. 启动 GHCi (交互式 REPL)
stack ghci

# 在 GHCi 中:
# > :load src/AST.hs        # 加载模块
# > :type expr              # 查看类型
# > :info Expr              # 查看类型定义
# > :quit                   # 退出
```

### 性能分析

```bash
# 编译带性能分析的版本
stack build --profile

# 运行并生成性能报告
stack exec -- lambda-interpreter-exe +RTS -p -s

# 查看报告
cat lambda-interpreter-exe.prof
```

---

## 1.4 Haskell Language Server (HLS)

HLS 提供:
- 实时类型检查
- 自动补全
- 跳转到定义
- 代码格式化
- Haddock 文档提示

### VS Code 配置

安装插件:
1. **Haskell** (官方插件)
2. **Haskell Syntax Highlighting**

配置 `.vscode/settings.json`:

```json
{
  "haskell.manageHLS": "GHCup",
  "haskell.formattingProvider": "fourmolu"
}
```

---

## 1.5 练习: 验证环境

### 任务 1: 安装并验证 Stack

```bash
# 安装 (如果还没安装)
# ... 使用上面的方法 ...

# 验证
stack --version

# 初始化项目 (已经为你创建好了,只需要运行)
stack build
```

**预期输出:**
```
lambda-interpreter-0.1.0.0: configure
lambda-interpreter-0.1.0.0: build
...
Completed 2 action(s).
```

### 任务 2: 测试 GHCi

```bash
stack ghci
```

在 GHCi 中输入:

```haskell
-- 基础表达式
1 + 2
-- 输出: 3

-- Lambda 表达式
(\x -> x + 1) 5
-- 输出: 6

-- 查看类型
:type (\x -> x + 1)
-- 输出: (\x -> x + 1) :: Num a => a -> a

-- 退出
:quit
```

### 任务 3: 创建你的第一个 Haskell 文件

创建 `test.hs`:

```haskell
-- TODO: 在这里写一个函数,计算列表长度
myLength :: [a] -> Int
myLength = undefined  -- 替换为你的实现

-- TODO: 写一个函数,判断一个数是否为偶数
isEven :: Int -> Bool
isEven = undefined  -- 替换为你的实现
```

运行:

```bash
stack ghci test.hs
```

在 GHCi 中测试:

```haskell
> myLength [1,2,3]
-- 应该输出: 3

> isEven 4
-- 应该输出: True
```

---

## 1.6 故障排除

### 问题 1: "stack: command not found"

**解决方案:** 将 Stack 添加到 PATH

```bash
# 添加到 ~/.bashrc 或 ~/.zshrc
export PATH="$HOME/.local/bin:$PATH"

# 重新加载配置
source ~/.bashrc  # 或 source ~/.zshrc
```

### 问题 2: 编译时网络错误

**解决方案:** 使用国内镜像

```bash
# 创建 ~/.stack/config.yaml
echo "urls:
  latest-snapshot: https://mirrors.tuna.tsinghua.edu.cn/haskell/stack/snapshots.json" > ~/.stack/config.yaml
```

### 问题 3: GHC 版本不匹配

**解决方案:** 切换到项目指定的 GHC 版本

```bash
stack setup
# 会自动下载 stack.yaml 中指定 resolver 的 GHC 版本
```

---

## 下一步

环境配置完成后,进入 **Phase 2: 抽象语法树 (AST)**

你将学习:
- 什么是代数数据类型 (ADT)
- 如何用 ADT 建模编程语言语法
- Haskell 的类型系统如何保证代码正确性

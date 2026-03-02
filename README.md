# LambdaInterpreter

A λ-calculus interpreter implemented in Haskell, featuring De Bruijn indexing and lazy evaluation.

这是一个用 Haskell 实现的 λ 演算解释器。它通过 **De Bruijn Index（德布勒因指数）** 解决了变量捕获问题，并采用**惰性求值（Lazy Evaluation）**策略。

## ✨ 特性 (Features)

* **De Bruijn Indexing**: 使用索引而非名称来管理变量，逻辑严密，避免命名冲突。
* **Lazy Evaluation**: 支持归约最外层的可归约式（Normal Order Reduction）。
* **REPL Environment**: 交互式命令行，支持环境绑定（`:let`）和自由变量展开。
* **Church Encoding**: 内置 I, K, S 组合子以及布尔值、丘奇数（Church Numerals）定义。

## 🚀 快速开始 (Getting Started)

### 编译 (Compilation)

确保你已安装 [GHC](https://www.haskell.org/ghc/)。在 `src` 目录下运行：

```bash
ghc --make Main.hs -o lambda
./lambda
```

启动解释器后，你可以直接输入 λ 表达式或使用命令：

```bash
λ> :let ONE = SUCC ZERO
λ> PRED ONE
结果: λx. λx'. x'
```

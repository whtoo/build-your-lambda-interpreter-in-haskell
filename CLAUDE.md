# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a **pedagogical Lambda Calculus Interpreter** implemented in Haskell - a project-based tutorial for learning advanced functional programming concepts. The interpreter implements a type-safe lambda calculus with Hindley-Milner type inference, serving as a comprehensive learning resource for programming language theory.

**Project characteristics:**
- Chinese-language learning project (comments and documentation in Chinese)
- Educational code with extensive TODOs for learners to implement
- Clean modular architecture with clear separation of concerns
- Built with Stack using LTS Haskell 21.25 (GHC 9.4.8)

## Common Development Commands

### Build and Test
```bash
stack build              # Build the entire project
stack build --fast       # Fast build (skip optimizations)
stack test               # Run all tests
stack test --coverage    # Run tests with coverage
stack run                # Start the REPL
stack ghci               # Enter interactive GHCi for development
stack clean              # Clean build artifacts
```

### Working with Specific Components
```bash
# Build library or executable only
stack build :lambda-interpreter:lib
stack build lambda-interpreter-exe

# Run specific test patterns
stack test --test-arguments="--pattern=AST"
stack test --test-arguments="-v"  # Verbose output

# Load specific module in GHCi
stack ghci
:load src/Parser.hs
:reload                  # After making changes
```

## Project Architecture

The project follows a clean **pipeline architecture** for language implementation:

```
Source Code → Parser → AST → TypeChecker → Evaluator → Result
                      ↓
                  REPL (interactive)
```

### Module Structure

**Core Modules (src/):**
- `AST.hs` - Abstract Syntax Tree definitions (Expr, Type types)
- `Parser.hs` - Megaparsec-based parser with error handling
- `TypeChecker.hs` - Hindley-Milner type inference algorithm
- `Evaluator.hs` - Beta reduction and WHNF evaluation
- `REPL.hs` - Interactive REPL with state management (Monad transformers)

**Entry Point (app/):**
- `Main.hs` - CLI argument handling and REPL startup

**Tests (test/):**
- `Spec.hs` - Tasty + QuickCheck test suite

### Key Design Patterns

1. **Algebraic Data Types**: Heavy use of ADTs for modeling language syntax (Expr, Type constructors)
2. **Parser Combinators**: Megaparsec for robust, composable parsing with proper error handling
3. **Monad Transformers**: REPL uses a stack of monads (StateT, ExceptT) for state and error management
4. **Type Classes**: OverloadedStrings for pretty printing integration
5. **Immutable Data**: Pure functional design throughout

### Type System Flow

The Hindley-Milner type inference works in phases:
1. **Generate Constraints**: Walk the AST to collect type constraints
2. **Unification**: Solve constraints using the unification algorithm
3. **Substitution**: Apply substitutions to get the principal type
4. **Generalization**: Handle polymorphic types (let-polymorphism)

### Evaluation Strategy

- **Weak Head Normal Form (WHNF)**: Evaluates function applications but not function bodies
- **Normal Order**: Leftmost-outermost reduction strategy
- **Beta Reduction**: Proper variable capture avoidance via alpha conversion

## Language Syntax

The implemented lambda calculus dialect supports:

**Lambda expressions:**
- Variables: `x`, `foo`, `bar_baz`
- Abstraction: `\x -> x + 1` or `lambda x. x + 1`
- Application: `f x` (left-associative: `f x y` ≡ `(f x) y`)

**Literals and operations:**
- Integers: `42`, `-5`
- Booleans: `true`, `false`
- Arithmetic: `1 + 2 * 3` (operator precedence: `*` > `+`)
- Conditionals: `if true then 1 else 0`

**Operator precedence (high to low):**
1. Function application
2. Multiplication (`*`)
3. Addition (`+`)
4. Conditional (`if`)

See `docs/language_syntax.md` for complete syntax specification and examples.

## Testing Framework

Tests use **Tasty** as the main framework with **QuickCheck** for property-based testing.

Test categories:
- Parser tests (correctness of parsing)
- Type checker tests (type inference validation)
- Evaluator tests (evaluation semantics)
- Integration tests (full pipeline)
- Property tests (mathematical properties via QuickCheck)

Run specific test categories by pattern matching the test name.

## Learning Structure

The project includes a 12-day curriculum in `docs/`:
- Day 1: Environment setup
- Days 2-3: ADTs and AST design
- Days 4-5: Parser combinators (Megaparsec)
- Days 6-8: Type inference (Hindley-Milner)
- Days 9-10: Expression evaluation
- Days 11-12: REPL and testing

Each module contains learning objectives in Haddock comments at the top of the file.

## Important Implementation Notes

1. **Incomplete Code**: Many functions are marked with TODOs in Chinese comments - this is intentional for learning purposes
2. **Error Handling**: Megaparsec provides detailed parse errors; TypeChecker has custom TypeError types
3. **Variable Capture**: Alpha renaming is used to avoid variable capture during substitution
4. **Occurs Check**: Implemented in TypeChecker to prevent infinite types
5. **Pretty Printing**: Uses `prettyprinter` library for consistent output formatting

## Dependencies

Key dependencies from `package.yaml`:
- `megaparsec >= 9.0` - Parser combinators
- `prettyprinter >= 1.7` - Pretty printing
- `containers >= 0.6` - Data structures (Map, Set)
- `mtl >= 2.2.2` - Monad transformers
- `tasty >= 1.4` - Test framework
- `QuickCheck >= 2.14` - Property-based testing

## IDE Support

The project includes `hie.yaml` for Haskell IDE Engine support, providing:
- Separate configuration per component (library, executable, tests)
- Path-based component mapping
- Stack-based project configuration

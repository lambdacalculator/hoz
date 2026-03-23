# Hoz: An Oz Interpreter in Haskell

Hoz is an implementation of a subset of the Oz programming language, supporting functional, declarative, and stateful threaded programming paradigms. It is designed for students in the "Programming Languages" course and follows the execution model described in **"Concepts, Techniques, and Models of Computer Programming" (CTM)**.

## Prerequisites
- **GHC** (Haskell Compiler) 9.0 or later
- **Cabal** (Haskell Build Tool) 3.0 or later

## Quick Start (For Students)

### 1. Installation
Clone the repository and install the `hoz` binary to your path:

```bash
git clone https://github.com/lambdacalculator/hoz.git
cd hoz
cabal install exe:hoz --overwrite-policy=always
```

This will symlink `hoz` to `~/.cabal/bin/hoz`. Make sure this directory is in your `PATH`.

### 2. Running your first program
Create a file named `hello.oz`:
```oz
local X in 
   X = 'Hello from Hoz!'
   {Browse X}
end
```

Run it:
```bash
hoz hello.oz
```

### 3. Keeping Hoz up to date
If your instructor releases updates or bug fixes, you can update your local copy easily:

```bash
./update.sh
```

## Quick Tips

1.  **Inspect Kernel Code**: If you are unsure why your code is failing, tell Hoz to output the elaborated Kernel syntax:
    ```bash
    hoz yourfile.oz yourfile.ozk
    ```
    Then inspect `yourfile.ozk` to see how your code was transformed (as described in CTM).

## Usage Guide

### Command Line Arguments
```bash
hoz [-q <quantum>] [-k] <input.oz> [output.ozk]
```

- `<input.oz>`: Source file. If extension is `.ozk`, executes as Kernel syntax directly.
- `[output.ozk]`: Optional output file for the elaborated Kernel AST.
- `-q <n>` / `--quantum <n>`: Set time slice for round-robin scheduling (default: infinity).
- `-c` / `--check`: Run state invariant checks after execution.

## Features & Documentation

### Supported Oz Subset
- **Variables**: Single-assignment dataflow variables.
- **Structures**: Lists, Records, Tuples.
- **Control**: `if`, `case`, `local`, `proc`, `fun`.
- **State**: `NewCell`, `Exchange` (and `:=`, `@` sugar).
- **Concurrency**: `thread`, `ByNeed` (lazy).
- **Exceptions**: `try/catch/raise`.

### Detailed Documentation
For a deep dive into primitives, debugging directives, and the execution model, see the **[User Manual](docs/Manual.md)**.

## Differences from Mozart/Oz
- No distribution/network layer.
- No constraint programming (`fd`).
- No object system (`class`, `Object`).
- Functors/Modules are not implemented.
- The `Browse` output is strictly monotonic and optimized for console viewing.

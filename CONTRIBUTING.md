# Contributing to Hoz

Thank you for your interest in improving Hoz! This project is used for the "Programming Languages" course, and we welcome improvements from TAs and students.

## Development Workflow

1.  **Build**: Use `make build` to compile the project.
2.  **Test**: Always run `make test` before submitting a Pull Request. This ensures that the core CTM examples still produce the expected output.
3.  **Elaboration**: If you modify the grammar or the elaboration logic in `Elab.hs`, pay close attention to the variable naming conventions in `README.md`.

## Project Structure

*   `hoz.hs`: Entry point and CLI parsing.
*   `Parser.hs`: Happy/Alex (or Parsec) parser for Oz and Kernel Oz.
*   `Elab.hs`: Elaboration of full Oz syntax into Kernel Oz.
*   `Semantics.hs`: Core execution logic (The Abstract Machine).
*   `Interp.hs`: Thread scheduling and execution loop.
*   `Primitives.hs`: Implementation of built-in functions.
*   `Kernel.hs`: AST definitions.

## Coding Standards

*   Keep functions small and well-typed.
*   Use descriptive variable names.
*   Document new primitives in `docs/Manual.md`.

## Regression Testing

The `examples/` directory contains standard Oz programs from the CTM textbook. The `.out` files are the expected outputs. If you make a change that intentionally alters output format, you must update these files using `make test`.

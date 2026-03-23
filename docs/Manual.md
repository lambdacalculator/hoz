# Hoz User Manual

This manual provides detailed information on using the `hoz` interpreter, including its command-line options, internal directives, supported primitives, and debugging tips.

## Command-Line Usage

```bash
hoz [options] <input_file> [kernel_output_file]
```

### Options

| Flag     | Long Flag       | Description                                               |
| :------- | :-------------- | :-------------------------------------------------------- |
| `-q <n>` | `--quantum <n>` | Set the thread scheduling quantum (default: infinity).    |
| `-k`     | `--kernel`      | Only accept files in kernel syntax.                       |
| `-c`     | `--check`       | Perform a store invariant check after execution finishes. |

---

## State Inspection Directives

Hoz supports "magic comments" or directives that can be placed in your Oz source code
to trigger state inspection during execution.

| Directive          | Description                                                                           |
| :----------------- | :------------------------------------------------------------------------------------ |
| `%show full`       | Prints the complete state: Thread Stack, Environment, Store (SAS), and Mutable Store. |
| `%show stack`      | Prints the current thread's execution stack.                                          |
| `%show globals`    | Lists all built-in primitives and their internal IDs.                                 |
| `%show mutable`    | Prints the Mutable Store (Cells).                                                     |

---

## Oz Language Coverage

Hoz accepts standard Oz syntax, as it appears in CTM, with a few exceptions:
* Strings are not supported, but constant strings can be simulated using single-quoted atoms: 'Hello, world!'
* Only a few primitive operations and built-in procedures are implemented (use `%show globals` to see the list)
* There is no `declare` command; use `local` or `in`-statements to introduce new variables into scope
* Modules, Functors, and various libraries are not implemented
* Message-Passing Concurrency is not supported (`NewPort`, `Send`)
* Shared-Memory Concurrency is not supported (`FailedValue`)
* Relational and Constraint Programming is not supported (`choice`, `fail`)
* Object-Oriented Programming is not supported (`class`, `New`, ...)
* Distributed Programming is not supported

### Output and Browser

In the Mozart/Oz system, the statement `{Browse X}` opens up a new window that monitors the value of X and updates it
with every change. I have created a similar implementation in Hoz, except that, instead of a new window, every change
to the monitored variable causes the current value of the variable to be printed to the console on a separate line.
A typical lines looks like this:

_1034: 0 | 1 | 2 | _

Here, 1034 is the store location where the variable is stored, and _ indicates an unbound tail to the list.
If this tail were bound to 3 | Y for some unbound variable Y, a new line would be printed:

_1034: 0 | 1 | 2 | 3 | _

Thunks are indicated with $. The rest should be self-explanatory.

---

## Debugging: Elaboration Variables

When `hoz` converts your Oz code into Kernel Oz ("elaboration"), it generates temporary variables.
Understanding these names can help when reading `.ozk` files or `%show` output:

*   `E<N>_`: Results of expressions (e.g., `X + Y`).
*   `P<N>_`: Temporary storage for pattern matching in `case`.
*   `A<N>_`: Argument variables for procedures/functions.
*   `C<N>_`: Cell values during exchange operations.
*   `X<N>_`: Variables captured in `catch` blocks.
*   `N<N>_`: Anonymous variables generated from `_`.

---

## Technical Notes

### Rational Trees
Hoz natively supports rational trees (cyclic structures). When printing a cyclic structure,
it uses back-references marked with identifiers like `(C1)`.

Example: `X=f(a:X)` will be printed as `(C1) f(a:C1)`.

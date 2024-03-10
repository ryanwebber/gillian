# Gillian
A stack-based code golfing language.

## Getting Started
```bash
# Building the project
cargo build --release

# Running the interpreter
echo '"Hello, World!"' | ./target/release/gillian
```

## Language Specification

A Gillan program is a plain ASCII string of characters. The program is executed by reading the
characters from left to right, and performing the corresponding operations.

As a stack-based language, most operations involve pushing and popping values from the stack.
Values left on the stack at the end of the program are printed to the console separated by new
lines. If the only remaining value is a list, it's elements are instead printed to the console,
separated by new lines. 

### Types
 - `Number` - A 64-bit floating point number.
 - `List` - An ordered sequence of values.

### Literals
 - `[0-9]+` pushes the corresponding base-10 number to the stack.

### Unary Operators
 - `[IXCM]+` multiply the value on the stack by `1`, `10`, `100`, or `1000` respectively,
   terminating a number literal if necessary. If the value on the stack is a list, the operation
    is performed element-wise.

### Binary Operators
 - `.` pops 2 values, `A` and `B`, off the stack and pushes an array containing `B` repeated `A` times.
 - `*` pops the top 2 values off the stack and multiplies them, pushing the result to the stack. If
   either value is a list, the multiplication is performed cartesian-product wise.

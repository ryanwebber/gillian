# Gillian
A stack-based code golfing language.

## Getting Started
```bash
# Building the project
cargo build --release

# Print the first 100 fibinacci numbers
echo 'p1pC{2P+' | ./target/release/gillian
#  p: Duplicate the top value on the stack. The stack is empty, so a 0 is pushed instead. The stack now contains [0].
#  1: Push the number 1 to the stack. The stack now contains [0, 1].
#  p: Duplicate the top value on the stack. The stack now contains [0, 1, 1].
#  C: Multiply the top value on the stack by 100. The stack now contains [0, 1, 100].
#  {: Pop 100 off the stack. Run the following block (implicitly terminated by EOF) 100 times.
#      2: Push the number 2 to the stack.
#      P: Pop 2 off the stack. Duplicate the top 2 values on the stack. The stack now contains [0, 1, 0, 1].
#      +: Pop the top 2 values off the stack and add them. The stack now contains [0, 1, 1].
#      ... The block repeats, copying the top 2 stack values and replacing them with their sum.
```

## Language Specification

A Gillan program is a plain ASCII string of characters. The program is executed by reading the
characters from left to right, and performing the corresponding operations.

As a stack-based language, most operations involve pushing and popping values from the stack.

### Types
 - `Number` - A 64-bit floating point number.
 - `List` - An ordered sequence of values.
 - `String` - A sequence of characters. Also represents blocks of code.

### Literals
| Literal  | Description |
|:--------:|-------------|
| `[0-9]+` | Pushes the corresponding base-10 number to the stack. |

### Range Operations
| Operator | Description |
|:--------:|-------------|
| `R`      | Pops a number `n` off the stack. Constructs a range from 0 to `n`, non-exclusive, and pushes it to the stack. |


### Unary Operators
| Operator | Description |
|:--------:|-------------|
| `[IXCM]` | Multiply the value on the stack by `1`, `10`, `100`, or `1000` respectively. If the value on the stack is a list, the operation is performed element-wise. `I` is useful to terminate number literals when necessary. |
| `p`      | Duplicates the top value on the stack and pushes it. |
| `A`      | Pops the top value off the stack, adds `1` to it, and pushes the result. If the value is a list, the operation is performed element-wise. |


### Binary Operators
| Operator | Description |
|:--------:|-------------|
| `.`      | Pops 2 values, `A` and `B`, off the stack and pushes an array containing `B` repeated `A` times. |
| `*`      | Pops the top 2 values off the stack and multiplies them, pushing the result to the stack. If either value is a list, the multiplication is performed cartesian-product wise. |
| `+`      | Pops the top 2 values off the stack and adds them, pushing the result to the stack. If either value is a list, the addition is performed cartesian-product wise. |
| `P`      | Pops a number `n` off the stack. Duplicate the top `n` values on the stack and push them in order. |

### Special Operators
| Operator | Description |
|:--------:|-------------|
| `$`      | Pops a string off the stack and evaluates it as source code on the existing stack. |

### Special Behaviors
 - **Termination**: Values left on the stack at the end of the program are printed to the console separated by new
   lines. If the only remaining value is a list, it's elements are instead printed to the console,
   separated by new lines.
 - **Underflow**: There is no underflow. If an operation requires more values than are on the stack, the number
   `0` is used in place of any missing values.
 - **Ranges**: When used in arithmetic operations, ranges are treated as lists containing the integers represented
   by the range.

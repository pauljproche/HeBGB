# HeBGB

CS107: Compilers Project <br>
Names: Craig Cagner, Sam Cohen, James Eidson, Paul Roche <br>
Emails: craig.cagner@tufts.edu, scohen10@tufts.edu, james.eidson@tufts.edu, paul.roche@tufts.edu <br>

## Installing Dune

Dune is required to build the compiler.
Go inside the src directory and then run the following commands:

`eval $(opam config env)` <br>
`opam install dune`

## Running the Top-Level Program:

To compile, run the following command: <br>
`make`<br>

The top-level program reads HeBGB code from stdin or a supplied filename, compiles it, and writes the compiled LLVM to stdout.

## Running the Compiler

A script, `HeBGB` is used to run the compiler. Run `HeBGB sourcefile.hebgb` where `sourcefile.hebgb` is the path to the source file to be compiled. The script will compile the source to a `.ll` file, compile that into a `.s` file, then link it with `builtin.o` which should have been built by `make`. By default, the `HeBGB` keeps the intermediate `.ll` and `.s` files.

*NOTE:*
The HeBGB script may need to be modifed to point to your installation of `llc`.

## Running the Test Scripts:

After `make`ing the project, run `./testall.sh` to run our test. `./testall.sh` just
uses the `HeBGB` script.

The following tests are included in the project:

| test name                          | output  | description                                                                               | Pass/Fail |
|------------------------------------|---------|-------------------------------------------------------------------------------------------|-----------|
| `apply_lambda.hebgb`               | 4       | applies a user-defined function                                                           | Pass      |
| `binding_failed.hebgb`             | error   | attempts to bind a value which has not been defined                                       | Fail      |
| `binding.hebgb`                    | 11      | test bindings                                                                             | Pass      |
| `captured_recursion.hebgb`         | 3628800 | makes a recursive call where the called function has been captured                        | Pass      |
| `capture.hebgb`                    | 9       | creates a local function and returns it closing over a single value in the environment    | Pass      |
| `curry.hebgb`                      | 6       | creates several local functions with nested closures                                      | Pass      |
| `factorial.hebgb`                  | 720     | tests basic recursion                                                                     | Pass      |
| `if_failed_type.hebgb`             | error   | asserts that the two branches of an if statement have the same type                       | Fail      |
| `infix_math.hebgb`                 | 19      | tests operator precedence and grouping on integers                                        | Pass      |
| `integer_compare.hebgb`            | -1      | tests comparisons on integers                                                             | Pass      |
| `print_in_function.hebgb`          | 5       | ensures that builtin functions can be called from inside lambda functions                 | Pass      |
| `print_zero.hebgb`                 | 0       | prints the integer constant 0 to stdout.                                                  | Pass      |
| `return_function_no_closure.hebgb` | 42      | creates a local function and returns it without closing over any value in the environment | Pass      |
| `two_ifs`                          | 20      | tests if expressions in combination with bindings                                         | Pass      |
| `wrong_arity.hebgb`                | error   | attempts to apply a function with the wrong number of arguments                           | Fail      |
| `wrong_function_body.hebgb`        | error   | defines a function where the function body does not match the return type                 | Fail      |
| `wrong_type_in_binding.hebgb`      | error   | attempt to bind a value which has a different type than the declared type                 | Fail      |

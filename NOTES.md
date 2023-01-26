# Notes

## TinyC Language

- functions can be defined only at the top level (global) - done, Parser
- structs (`AstStructDecl`) & typedefs (`AstFunPtrDecl`) have their own namespace
- variables and functions are lexically scoped - done, SemanticAnalysis
  - one name cannot be declared twice on the same level (same lexical frame) once as a function and once as a variable - done, SemanticAnalysis
- global variables, functions and structs can have one or multiple forward declarations - done, SemanticAnalysis
  - all of them must be type compatible
  - there can be only one definition
- `switch` statement must contain unique case values - done, SemanticAnalysis
- function declarations must contain unique argument names - done, SemanticAnalysis
- only complete struct types can be used in the program
  - the struct is available as an incomplete type in its own definition
  - pointers are always complete, so recursive structs are possible
- assignment must contain lvalue on left side
- recursive function pointers are not allowed (function cannot take or return pointer to the same type)

```c
int foo(double); // declaration
int foo(double x){ return x; } // definition
```

### Casting

- implicit casts:
  - `char` <-> `int`
  - `char` <-> `double`
  - `int` <-> `double`
  - any`*` -> `void *` (can assign any pointer to `void*` variable/argument)

- explicit casts (`cast<Ty>(Expr)`)
  - all implicit casts
  - `void*` -> any`*`
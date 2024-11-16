# Resolver

This crate handles name resolution, it operates directly at the 
Abstract Syntax Tree level.

Basically, it turns variables and functions names into indices,
refering to a slot in a symbol table.

In this table (of type: `SymbolTable`) we store:
* the original name of the variable,
* where it was declared,
* if the value binded to it was captured by a closure or not (we call such values "upvalues")

This process is done in two steps:
* first we traverse the root statements of the input script, in this shallow pass
  we can collect all globals.
* then we traverse the entire AST, while maintaining a stack of all defined variables, per scope.
  by doing so we can easily track what variables are refering to,
  and spot the usage of undefined  variables.
  In the code, the stack of variable definitions is represented by the type `ScopeChain`.

The first step is necessary because the implementation of a function can refer to a global
defined later in the script. As long as the global definition is executed prior to the function call, 
this is valid.

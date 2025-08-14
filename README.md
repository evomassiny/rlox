This repo contains a WIP implementation of the `lox` programming language,
it is inspired by the `clox` implementation of the *Crafting interpreters* book.

My end goal is to build an `x86_64` elf binary from a lox script, 
respecting the language semantics. Similar to what `mypyc` does for python. 

However the roadmap is not clear to me, **this is a learning project**, and I will most likely never complete it :)

My current plan is to perform as much static analysis as I can on the input script,
such as:
* name resolution
* upvalues identification
* type checking
* dead code elimination,
* constant folding
* lifetime analysis

Then, somehow come up with an optimal representation of all user-defined types,
by turning dict based objects into structs.

Finally, when I've came up with an optimized Control Flow Graph, and an optimal
representation of all user type, lower it down into assembly, and link the result
with some kind of runtime for the dynamic parts, including a garbage collector.
As you may have guessed, all of it is purely hypotethical for now ^^

Contrary to the original `clox` implementation, 
this is not a simple pass compiler, most passes are handled by a dedicated crate:
* the `lexer` tokenizes the input source ([doc](./lexer/README.md))
* the `parser` parses the tokens into a basic Abstract Syntax Tree ([doc](./parser/README.md))
* the `resolver` resolves names on the AST ([doc](./resolver/README.md))
* the `type_checker` performs type checking on the ~~symbols~~, ~~CFG~~, **AST** ([doc](./resolver/README.md))
* the `control_flow_graph` turns the __AST__ into a __Control Flow Graph__ (CFG), it also turns variables
  into an __Static Single Assigment__ form (__SSA__) ([doc](./control_flow_graph/README.md))

The language implementation has some restrictions relative to the original one:
* Classes must be defined in the top-most scope, conditional Type definition is not allowed,
  (it makes type checking much much simplier)

For the runtime/VM part, so far, it differs from the original implementation on the following points:
* it uses a compacting garbage collector based on the "immix" implementation,
  ([related doc](./heap/README.md)).

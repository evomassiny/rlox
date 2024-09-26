This repo contains a WIP implementation of the `lox` programing language,
it is inspired by the `clox` implentation of the *Crafting interpreters* book.

So far, it differs from the original implementation on the following points:
* it uses a compacting garbage collector based on the "immix" implementation,
  ([related doc](./heap/README.md)).
* it is not a simple pass compiler, here are the main compiler passes:
  * tokenize the input source ([doc](./lexer/README.md))
  * parse the tokens into a basic Abstract Syntax Tree ([doc](./parser/README.md))
  * resolve names on the AST ([doc](./resolver/README.md))
  * High Level Intermediate Representation 
  * Typed High Level Intermediate Representation
  ...
  * profit



# About

This is an handcrafted pratt parser, following closely the BNF grammar of the __lox__
programming language.

To be entirely exact, statements are parsed in a dead simple recursive descent parser
while expressions are parsed by the pratt parser.

This parser consumes a stream of tokens, as produced by the lexer.

It outputs an Abstract Syntax Tree (AST), which closely mimicks 
the source inputs.

At this stage, we havn't done any semantic analysis on the AST,
said otherwise, we can't trust the AST produced by this parser to represent a
valid program.


The produced AST is represented by a nesting of the `Stmt` (for statement) type.
Each node might contain a tree of `Expr` (for expressions).

All big attributes of `Stmt`s and `Expr`s are boxed to keep the node size small.


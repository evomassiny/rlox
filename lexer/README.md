# About

This lexer is an hand-crafted _Deterministic Finite Automaton_,
(eg: "a bunch of nested if statements").

It supports 'utf-8'.

It's meant to be generic over 2 kind of input:
* `&str`
* files

Input reading, utf-8 decoding and token scanning are done in a streaming fashion.

The generated token (of type `Token`) contain two main attributes:
* their __kind__ (`TokenKind`), eg: what they represent (parenthesis, identifier...)
* their __span__ (`Span`), eg: where in the input file they were generated, this is useful later for error generation.

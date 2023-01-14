# About

This lexer is an hand-crafted _Deterministic Finite Automaton_,
(eg: "a bunch of nested if statements").

It supports 'utf-8'.

It's meant to be generic over 2 kind of input:
* `&str`
* files

Input reading, utf-8 decoding and token scanning are done in a streaming fashion.

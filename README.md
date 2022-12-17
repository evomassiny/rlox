This repo contains a WIP implementation of the `lox` programing language,
it is inspired by the `clox` implentation of the *Crafting interpreters* book.

So far, it differs from the original implementation on the following points:
* it uses a compacting garbage collector based on the "immix" implementation,
  ([related doc](./heap/README.md)).

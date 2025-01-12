# Control flow graph

This crates handles:
* the transformation from an Abstract Syntax Tree to a Control Flow Graph,
* turning variables into Static Single Assigment form.

This pass is meant to run __after__ name resolution.

The CFG representation of the code will hopefully ease the execution 
of later statical analysis passes, most notably: __type checking__.

Later stages of the compiler should use this CFG representation of the source 
(or another one derived from it).

Ressources: the `Mir` struct from inko's compiler was of great help.

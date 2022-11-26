# Heap
This crate loosely follows the implemenation of the "Immix" garbage collector,
a "mark and compact" GC.

Here is how it works:
* we allocate 32kiB `Block`s of data on demand,
* the VM claims chunks of those blocks when allocating new objects,
* from time to time, we stop the world and mark all live objects,
* when marking we annotate each block with few statistics describing its usage,
* when we detect `fragmented` blocks, we mark them as such.
* In the following marking pass, when we encounter an object lying in a `fragmented`block,
  we copy it into a non-`fragemented` block, and leave in its place a reference to the 
  new location.
* While traversing the objects graph in the marking phase, when we encounter "reference to a new   object location", we update the refs of the objects so they point to the relocated one.

The step of relocating objects from fragemented blocks into other ones is called `evacuation`.

To estimate the fragmentation of each block, we divide it into `lines` of 128 bits, 
and mark them when they contain live objects.

# references
See the original [immix paper](https://www.cs.utexas.edu/users/speedway/DaCapo/papers/immix-pldi-2008.pdf), 
and a [video of the author describing it](https://www.youtube.com/watch?v=73djjTs4sew).

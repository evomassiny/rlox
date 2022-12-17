# Heap
This crate loosely follows the implemenation of the "Immix" garbage collector,
a "mark and compact" GC.

Here is how it works:
* we allocate 32kiB `Block`s of data on demand,
* the VM claims chunks of those blocks when allocating new objects,
* from time to time, we stop the world and mark all live objects,
* when marking we annotate each block with few statistics describing its usage,
* when we detect `fragmented` blocks, we mark them as such.
* In the following marking pass, when we encounter an object lying in a `fragmented` block,
  we copy it into a non-`fragemented` block, and leave in its place a reference to the 
  new location.
* While traversing the objects graph in the marking phase, 
  when we encounter "reference to a new object location",
  we update the refs of the objects so they point to the relocated one.

The step of relocating objects from fragemented blocks into other ones is called `evacuation`.

To estimate the fragmentation of each block, we divide it into `lines` of 128 bits, 
and mark them when they contain live objects.

# About Blocks

Heap allocates memory by `Block`s, a block contain:
 * a bunch of live objects (Buffers, Numbers, ...), sparsely located in the block memory,
 * a `BlockHeader` that describes how "full" a block is.

`BlockHeader`s are located on the very start of each block,
and `Block` are **aligned on their size**.

This makes it trivial to obtain the address of the `BlockHeader` 
related to the `Block` where an object is allocated
from the object address (by filtering out the lower bits of the
object address).

This implementation relies (a lot) on this property.


# About heap allocated Objects

All objects allocated in a block is annotated with a `Header`,
we store the type the of the object inside, along with a "mark" flag (used it the GC cycle).


# Heuristics for "evacuation"

To compact the heap, we move all object of certain blocks into others,
we end up with free blocks and fuller blocks, we call this "evacuating" a block.

We need some kind of heuristic to find out which block to free, and which one to keep.

"immix" choses to categorize blocks by the number of _holes_ they contain (eg: 
contiguous empty memory slots), it picks a _hole count threshold_ and 
evacuates all blocks containing **more** holes into blocks with **less** holes.

We pick a _hole count threshold_ that maximizes the number of block to evacuate while
asserting that the evacuated objects can still fit in the remaining blocks.

With this method, we're sure to evacuate the "most fragmented" blocks first (eg: those with the
biggest number of _holes_), without having to allocate a new block.

# References
See the original [immix paper](https://www.cs.utexas.edu/users/speedway/DaCapo/papers/immix-pldi-2008.pdf), 
and a [video of the author describing it](https://www.youtube.com/watch?v=73djjTs4sew).

# Specification for the Alloca Module

### General Design of the Arena Allocator

Small objects are placed in arenas. (An arena is a region in memory where we
place live objects. An arena is dropped when every object in the arena has died).

With this in mind, we can draw a very simple conclusion: for fast garbage
collection, we need a very fast way to get the arena by a pointer.

All right, let arena_size = 1 << x. If arenas are placed in memory continuous,
and we know the pointer to the start of this block (A block is a sequence of
arenas) we can find the index of an arena in the block (an index like index in
an array, where each arena is an element of the array) 
using: index = (ptr - block_start) >> x.

But we cannot place all objects in a single block because a big arena with
many small objects will live for a very long time, and big objects cannot be
in a small arena.

Well, big objects go to big arenas, small to small etc. medium - medium. 
But in one block we can store only arenas with the same size, with this in mind,
we need different blocks. But if we have more than one blocks, we can only
quickly find an arena inside one of block.

Also, we can not create one large block for every arena size, because we need
a continuous region of memory for each block, with this in mind we cannot expand
the size of a block, and we are forced to storage arenas with the same size in
different blocks, and when all arenas in a block are full, create a new block.

Okay, we have new challenge, how dow we get block_by_ptr? Very easy, we reserve
a very large region for us, but small for the virtual address space (like 128 gb),
and split it into blocks (with size like 64mb), and will keep an array of 
`Option<&'a HedgeBlock>` (`HedgeBlock` is an implementation of an abstract Block)
and inside each struct Block, we keep block_start and the arena size for this
block for fast arena_by_ptr.

### The alloc method.

We always want to place object in arena with the smallest amount of memory,
but still more than object requires. In this situation we need a data structure
with fast insert, remove and lower_bound (for find an arena for object)
operation. Let's give a name for this data structure `Heap`. In this `Heap` we
will keep a reference to struct Arena, which has a field arena_start, arena_cur etc.

A trivial optimization for our data structure is lazy inclusion of arenas 
(inclusion of arena is means allocating memory from arena_start with len
arena_size and addition this arena to the `Heap`) in other words, it makes sense
to keep only one empty arena for which size.

For example, at program startup, the array of `Option<&'a HedgeBlock>` is
initialized with `None`, and for first allocation we create new block, with
arena_size = min(SIZES_FOR_ARENA) (like 512b for 24b object or 4kb for 4kb array),
create an arenas, inclusion one them and return a pointer to start of arena
(and update arena_cur)

In my implementation, when constructing of `HedgeBlock`, I also construct all
`HedgeArena`s for this block and create two arrays: `items` and `slots`. 
In `items`, I keep reference to `HedgeArena`s in memory order, so that
`arena_by_ptr` can find them in O(1). In `slots`, I keep a died arenas, and to
choose a new arena for inclusion, i simply `pop` one. 
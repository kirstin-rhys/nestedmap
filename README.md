# Nested Map

Where the fundamental signature for map is `k -> v`, for a nested map
it's `[k] -> [v]`. Or perhaps slightly more accurately, something like
`k -> k -> k -> ... -> [v]`.

One way of thinking about Map is as the tabulation or memorization of
a function, or perhaps the transformation from 'time' to 'space'. In
the first conception, we could think about NestedMap as the shape
preserving transformation of the composition of functions; in the
second interpretation, we could view NestedMap as a shape preserving
transformation of structured time to structured space.

Alternately, we could just look it as nice implemetation of parametric
sparse tensor.

For version 2.0, it would be nice to change `[(k,v)]` into a
heterogenous list. For now, this can be poorly addressed
using sum or existential types.

## How is this at all useful?

Well, for example, it makes the implementation of an efficent markov
chain trivial.

It's also useful for eliminating bulky conditions on transformations
of complex data. By first transforming the data into a nested map
representation via a list of projections, it's much easier write the
logic for tranforming and operating on the data--since at any
particular node in the nested map, you are guaranteed to not only be
working with data described by the ancestors, but also all of the
prescribed data.

Another example might be delineating highways from local roads given a
data set of directions, after doing a topological sort.

It is very useful for any sort of number crunching on
sparse matricies or tensors.

## Why not just use Map [k] v?

Well, we could of course. However, we're often very interested in our
context--the coordinates of where we live. While Map [k] v could make
this theoretically possible, it's horrible inefficent, and would make
it much harder to guarentee structural soundness.

For example, we could easily delete an interior node while leaving the
descendents intact. That's just not possible with NestedMap.

## Isn't this just a Trie?

Well, it kind of is. But in practice, tries seem to be focused on
strings as keys. We want to be more general. I'm sure there is a
better name.

## Isn't this just a RoseTree?

Well, it kind of is. But the RoseTree is just `RoseTree a`, where as
we are `Tree k a` or `Forest k a`. Of course, you could use a pair and
define the Ord and Eq instance appropriately, but it's using lists
underneath, which is a performance killer for large data sets.

## Implementation Notes

So, there are two interfaces: Tree and Forest.

Neither one is favored over the other. Use Tree if you need a top level value.

Theoretically, these mutually recursive shapes should be completely
independent of each other, perhaps just exposed to the other via a
typeclass. However, mutually recursive modules are very difficult to
work with, even with GHC. Perhaps one day. In the mean time, the
implementation doesn't matter so much and won't affect client code as
long as the public Tree and Forest interfaces are used.

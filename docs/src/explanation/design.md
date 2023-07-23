# Design choices

There are many ways to implement geometric algebra numerically, and each performs its own trade-offs, depending on the goals it is set to achieve.

In this library, we seek to allow the use of geometric algebra as a language to express logic. In particular, having a concrete representation of the objects and operators of geometric algebra is not a necessity, as long as the relevant operations are easy to express. This is relevant in context of the integration with existing codebases; we chose not to require the use of library-specific types nor, for most cases, to extend specific methods. The interface with this library is based around the way data must be cast into the semantics of geometric algebra. This should therefore not affect the data structures you choose; be it a tuple, vector or static array, we only need a way to map data to components (see [this how-to](@ref howto-integration)).

Module hierarchy:

Module Data.ArrayBZ.Internals.MArray defines class MArray and general algorithms
working with any instance of this class

Module Data.ArrayBZ.Internals.IArray defines class IArray and general algorithms
working with any instance of this class. In addition, it defines functions
used to implement Show, Eq and Ord instances (these instances cannot be
defined here due to the "overlapped instances" problem)

Module Data.ArrayBZ.Internals.Boxed imports modules IArray and MArray, and then
defines Array/IOArray/STArray. Module Data.ArrayBZ.Internals.Unboxed also imports
modules IArray and MArray, and then defines UArray/IOUArray/STUArray

Modules Data.ArrayBZ.Boxed and Data.ArrayBZ.Unboxed reexports corresponding
internal modules together with IArray and MArray modules, what gives their users
ability to use all boxed (unboxed) arrays together with all general algorithms
defined for IArray and MArray classes

Also, file unused.hs contains parts of original code that's still not used in
new one

{-# OPTIONS_GHC -cpp -fglasgow-exts #-}
{- |
   Module     : Data.ArrayBZ.Internals.IArray
   Copyright  : (c) The University of Glasgow 2001 & (c) 2006 Bulat Ziganshin
   License    : BSD3

   Maintainer : Bulat Ziganshin <Bulat.Ziganshin@gmail.com>
   Stability  : experimental
   Portability: GHC/Hugs

Immutable arrays: class, general algorithms and Show/Ord/Eq implementations

-}

module Data.ArrayBZ.Internals.IArray where

import Control.Monad.ST         ( ST, runST )
import Data.Ix
#ifdef __GLASGOW_HASKELL__
import GHC.Arr                  ( unsafeIndex )
#endif
#ifdef __HUGS__
import Hugs.Array               ( unsafeIndex )
#endif

-----------------------------------------------------------------------------
-- Class of immutable arrays

-- | Class of array types with immutable bounds
-- (even if the array elements are mutable).
class HasBounds a where
    -- | Extracts the bounds of an array
    bounds :: Ix i => a i e -> (i,i)

{- | Class of immutable array types.

An array type has the form @(a i e)@ where @a@ is the array type
constructor (kind @* -> * -> *@), @i@ is the index type (a member of
the class 'Ix'), and @e@ is the element type.  The @IArray@ class is
parameterised over both @a@ and @e@, so that instances specialised to
certain element types can be defined.
-}
class HasBounds a => IArray a e where
    unsafeArray      :: Ix i => (i,i) -> [(Int, e)] -> a i e
    unsafeAt         :: Ix i => a i e -> Int -> e
    unsafeReplace    :: Ix i => a i e -> [(Int, e)] -> a i e
    unsafeAccum      :: Ix i => (e -> e' -> e) -> a i e -> [(Int, e')] -> a i e
    unsafeAccumArray :: Ix i => (e -> e' -> e) -> e -> (i,i) -> [(Int, e')] -> a i e

-----------------------------------------------------------------------------
-- Algorithms on immutable arrays

{-# INLINE array #-}
{-|
Constructs an immutable array from a pair of bounds and a list of
initial associations.

The bounds are specified as a pair of the lowest and highest bounds in
the array respectively.  For example, a one-origin vector of length 10
has bounds (1,10), and a one-origin 10 by 10 matrix has bounds
((1,1),(10,10)).

An association is a pair of the form @(i,x)@, which defines the value of
the array at index @i@ to be @x@.  The array is undefined if any index
in the list is out of bounds.  If any two associations in the list have
the same index, the value at that index is implementation-dependent.
(In GHC, the last value specified for that index is used.
Other implementations will also do this for unboxed arrays, but Haskell
98 requires that for 'Array' the value at such indices is bottom.)

Because the indices must be checked for these errors, 'array' is
strict in the bounds argument and in the indices of the association
list.  Whether @array@ is strict or non-strict in the elements depends
on the array type: 'Data.Array.Array' is a non-strict array type, but
all of the 'Data.Array.Unboxed.UArray' arrays are strict.  Thus in a
non-strict array, recurrences such as the following are possible:

> a = array (1,100) ((1,1) : [(i, i * a!(i-1)) | i \<- [2..100]])

Not every index within the bounds of the array need appear in the
association list, but the values associated with indices that do not
appear will be undefined.

If, in any dimension, the lower bound is greater than the upper bound,
then the array is legal, but empty. Indexing an empty array always
gives an array-bounds error, but 'bounds' still yields the bounds with
which the array was constructed.
-}
array   :: (IArray a e, Ix i)
        => (i,i)        -- ^ bounds of the array: (lowest,highest)
        -> [(i, e)]     -- ^ list of associations
        -> a i e
array (l,u) ies = unsafeArray (l,u) [(index (l,u) i, e) | (i, e) <- ies]

-- Since unsafeFreeze is not guaranteed to be only a cast, we will
-- use unsafeArray and zip instead of a specialized loop to implement
-- listArray, unlike Array.listArray, even though it generates some
-- unnecessary heap allocation. Will use the loop only when we have
-- fast unsafeFreeze, namely for Array and UArray (well, they cover
-- almost all cases).

{-# INLINE listArray #-}
-- | Constructs an immutable array from a list of initial elements.
-- The list gives the elements of the array in ascending order
-- beginning with the lowest index.
listArray :: (IArray a e, Ix i) => (i,i) -> [e] -> a i e
listArray (l,u) es = unsafeArray (l,u) (zip [0 .. rangeSize (l,u) - 1] es)

{-# INLINE (!) #-}
-- | Returns the element of an immutable array at the specified index.
(!) :: (IArray a e, Ix i) => a i e -> i -> e
arr ! i = case bounds arr of (l,u) -> unsafeAt arr (index (l,u) i)

{-# INLINE indices #-}
-- | Returns a list of all the valid indices in an array.
indices :: (HasBounds a, Ix i) => a i e -> [i]
indices arr = case bounds arr of (l,u) -> range (l,u)

{-# INLINE elems #-}
-- | Returns a list of all the elements of an array, in the same order
-- as their indices.
elems :: (IArray a e, Ix i) => a i e -> [e]
elems arr = case bounds arr of
    (l,u) -> [unsafeAt arr i | i <- [0 .. rangeSize (l,u) - 1]]

{-# INLINE assocs #-}
-- | Returns the contents of an array as a list of associations.
assocs :: (IArray a e, Ix i) => a i e -> [(i, e)]
assocs arr = case bounds arr of
    (l,u) -> [(i, unsafeAt arr (unsafeIndex (l,u) i)) | i <- range (l,u)]

{-# INLINE accumArray #-}
{-|
Constructs an immutable array from a list of associations.  Unlike
'array', the same index is allowed to occur multiple times in the list
of associations; an /accumulating function/ is used to combine the
values of elements with the same index.

For example, given a list of values of some index type, hist produces
a histogram of the number of occurrences of each index within a
specified range:

> hist :: (Ix a, Num b) => (a,a) -> [a] -> Array a b
> hist bnds is = accumArray (+) 0 bnds [(i, 1) | i\<-is, inRange bnds i]
-}
accumArray :: (IArray a e, Ix i)
        => (e -> e' -> e)       -- ^ An accumulating function
        -> e                    -- ^ A default element
        -> (i,i)                -- ^ The bounds of the array
        -> [(i, e')]            -- ^ List of associations
        -> a i e                -- ^ Returns: the array
accumArray f init (l,u) ies =
    unsafeAccumArray f init (l,u) [(index (l,u) i, e) | (i, e) <- ies]

{-# INLINE (//) #-}
{-|
Takes an array and a list of pairs and returns an array identical to
the left argument except that it has been updated by the associations
in the right argument.  For example, if m is a 1-origin, n by n matrix,
then @m\/\/[((i,i), 0) | i \<- [1..n]]@ is the same matrix, except with
the diagonal zeroed.

As with the 'array' function, if any two associations in the list have
the same index, the value at that index is implementation-dependent.
(In GHC, the last value specified for that index is used.
Other implementations will also do this for unboxed arrays, but Haskell
98 requires that for 'Array' the value at such indices is bottom.)

For most array types, this operation is O(/n/) where /n/ is the size
of the array.  However, the 'Data.Array.Diff.DiffArray' type provides
this operation with complexity linear in the number of updates.
-}
(//) :: (IArray a e, Ix i) => a i e -> [(i, e)] -> a i e
arr // ies = case bounds arr of
    (l,u) -> unsafeReplace arr [(index (l,u) i, e) | (i, e) <- ies]

{-# INLINE accum #-}
{-|
@accum f@ takes an array and an association list and accumulates pairs
from the list into the array with the accumulating function @f@. Thus
'accumArray' can be defined using 'accum':

> accumArray f z b = accum f (array b [(i, z) | i \<- range b])
-}
accum :: (IArray a e, Ix i) => (e -> e' -> e) -> a i e -> [(i, e')] -> a i e
accum f arr ies = case bounds arr of
    (l,u) -> unsafeAccum f arr [(index (l,u) i, e) | (i, e) <- ies]

{-# INLINE amap #-}
-- | Returns a new array derived from the original array by applying a
-- function to each of the elements.
amap :: (IArray a e', IArray a e, Ix i) => (e' -> e) -> a i e' -> a i e
amap f arr = case bounds arr of
    (l,u) -> unsafeArray (l,u) [(i, f (unsafeAt arr i))
                               | i <- [0 .. rangeSize (l,u) - 1]]
{-# INLINE ixmap #-}
-- | Returns a new array derived from the original array by applying a
-- function to each of the indices.
ixmap :: (IArray a e, Ix i, Ix j) => (i,i) -> (i -> j) -> a j e -> a i e
ixmap (l,u) f arr =
    unsafeArray (l,u) [(unsafeIndex (l,u) i, arr ! f i) | i <- range (l,u)]

-----------------------------------------------------------------------------
-- Implementation of Show instance

{-# SPECIALISE
    showsIArray :: (IArray a e, Ix i, Show i, Show e) =>
                   Int -> a i e -> ShowS
  #-}

showsIArray :: (IArray a e, Ix i, Show i, Show e) => Int -> a i e -> ShowS
showsIArray p a =
    showParen (p > 9) $
    showString "array " .
    shows (bounds a) .
    showChar ' ' .
    shows (assocs a)

-----------------------------------------------------------------------------
-- Implementation of Eq/Ord instances

{-# INLINE eqIArray #-}
eqIArray :: (IArray a e, Ix i, Eq e) => a i e -> a i e -> Bool
eqIArray arr1 arr2 =
    case bounds arr1 of { (l1,u1) ->
    case bounds arr2 of { (l2,u2) ->
    if rangeSize (l1,u1) == 0
      then rangeSize (l2,u2) == 0
      else l1 == l2 && u1 == u2 &&
           and [ unsafeAt arr1 i == unsafeAt arr2 i
               | i <- [0 .. rangeSize (l1,u1) - 1]]}}

{-# INLINE cmpIArray #-}
cmpIArray :: (IArray a e, Ix i, Ord e) => a i e -> a i e -> Ordering
cmpIArray arr1 arr2 = compare (assocs arr1) (assocs arr2)

{-# INLINE cmpIntIArray #-}
cmpIntIArray :: (IArray a e, Ord e) => a Int e -> a Int e -> Ordering
cmpIntIArray arr1 arr2 =
    case bounds arr1 of { (l1,u1) ->
    case bounds arr2 of { (l2,u2) ->
    if rangeSize (l1,u1) == 0 then if rangeSize (l2,u2) == 0 then EQ else LT else
    if rangeSize (l2,u2) == 0 then GT else
    case compare l1 l2 of
        EQ    -> foldr cmp (compare u1 u2) [0 .. rangeSize (l1, min u1 u2) - 1]
        other -> other } }
    where
    cmp i rest = case compare (unsafeAt arr1 i) (unsafeAt arr2 i) of
        EQ    -> rest
        other -> other

{-# RULES "cmpIArray/Int" cmpIArray = cmpIntIArray #-}


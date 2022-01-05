{-# OPTIONS_GHC -cpp -fglasgow-exts -fallow-undecidable-instances #-}
{- |
   Module     : Data.ArrayBZ.Dynamic
   Copyright  : (c) The University of Glasgow 2001 & (c) 2006 Bulat Ziganshin
   License    : BSD3

   Maintainer : Bulat Ziganshin <Bulat.Ziganshin@gmail.com>
   Stability  : experimental
   Portability: Hugs/GHC

Arrays with dynamically changed bounds in IO and ST monads.

-}

module Data.ArrayBZ.Dynamic (
   -- $info
   -- * Type constructors
   Dynamic,
   DynamicIO,
   DynamicIOArray,
   DynamicIOUArray,
   DynamicST,
   DynamicSTArray,
   DynamicSTUArray,
   GrowBoundsF,
   -- * Operations
   newDynamicArray,
   newDynamicArray_,
   resizeDynamicArray,
   -- * Array growing strategies
   noGrow,
   growMinimally,
   growTwoTimes,
   -- Reexported MArray interface
   module Data.ArrayBZ.Internals.MArray
 ) where

import Data.Ref
import Data.ArrayBZ.Internals.MArray
import Data.ArrayBZ.IO
import Data.ArrayBZ.ST
#ifdef __GLASGOW_HASKELL__
import GHC.Arr                  ( unsafeIndex )
#endif
#ifdef __HUGS__
import Hugs.Array               ( unsafeIndex )
#endif

{- $info

Array with dynamically changed bounds can be created from any mutable array
type by using type converter Dynamic. I have created synonyms for widely used
array constructors, for example "DynamicIOUArray Int Double".
Dynamic array supports the same MArray and HasMutableBounds interfaces as other
mutable arrays, but they don't support HasBounds interface. Dynamic array can be
resized explicitly by operation `resizeDynamicArray`.

Dynamic array can also grow automatically when `writeArray` is used with index
that is out of current array bounds. For this to work, array should be created
using non-standard operations `newDynamicArray` or `newDynamicArray_`. The first
argument of these operations is "growing strategy", i.e. the function of type
`GrowBoundsF i`, other arguments are the same as for newArray/newArray_. The
predefined growing strategies include `noGrow` that disables automatic growing,
`growMinimally` that extends array only to include new index and `growTwoTimes`
that extend array at least 2 times each time it needs to grow.

When array grows, either explicitly or automatically, new elements are
initialized with `init` value if this array was created by
newArray/newDynamicArray.

-}

-- ---------------------------------------------------------------------------
-- Types

-- | Representation of dynamic array. Includes
--     * function to calculate new array bounds when it needs to grow
--     * optional value used for initializing new elements when array grows
--     * reference to current array contents
data Dynamic r a i e = Dynamic (GrowBoundsF i) (Maybe e) (r (a i e))

-- | Dynamic arrays in IO monad
type DynamicIO         = Dynamic IORef
-- |Dynamic version of IOArray
type DynamicIOArray    = DynamicIO IOArray
-- |Dynamic version of IOUArray
type DynamicIOUArray   = DynamicIO IOUArray

-- | Dynamic arrays in ST monad
type DynamicST       s =  Dynamic (STRef s)
-- |Dynamic version of STArray
type DynamicSTArray  s = (DynamicST s) (STArray  s)
-- |Dynamic version of STUArray
type DynamicSTUArray s = (DynamicST s) (STUArray s)

-- | This type represents function that calculates new array bounds when it needs to grow
type GrowBoundsF i  =  (i,i) -> i -> (i,i)

-- ---------------------------------------------------------------------------
-- Operations

-- | Create new dynamic array with default value for new cells set to `init`.
--   `f` is a growing strategy and may be `noGrow`, `growMinimally`
--    or `growTwoTimes`
newDynamicArray f bounds init = do
    arr <- newArray  bounds init
    a   <- newRef arr
    return (Dynamic f (Just init) a)

-- | Create new dynamic array where all new cells will remain uninitialized.
--   `f` is a growing strategy and may be `noGrow`, `growMinimally`
--    or `growTwoTimes`
newDynamicArray_ f bounds = do
    arr <- newArray_  bounds
    a   <- newRef arr
    return (Dynamic f Nothing a)

-- | Extend/shrink dynamic array to new bounds
resizeDynamicArray (Dynamic _ e a) newbounds = do
    arr <- readRef a
    bounds <- getBounds arr
    newarr <- case e of
                Just init -> newArray  newbounds init
                Nothing   -> newArray_ newbounds
    sequence_ [ readArray arr i >>= writeArray newarr i
              | i <- range bounds, inRange newbounds i ]
    writeRef a newarr

-- ---------------------------------------------------------------------------
-- Instances

instance (HasMutableBounds a m, Ref m r) => HasMutableBounds (Dynamic r a) m where
    {-# INLINE getBounds #-}
    getBounds (Dynamic _ _ a)  =  readRef a >>= getBounds

instance (MArray a e m, Ref m r) => MArray (Dynamic r a) e m where
    newArray_ =  newDynamicArray_ noGrow
    newArray  =  newDynamicArray  noGrow

    {-# INLINE unsafeRead #-}
    unsafeRead  (Dynamic _ _ a) i = do arr <- readRef a
                                       unsafeRead arr i

    {-# INLINE unsafeWrite #-}
    unsafeWrite (Dynamic _ _ a) i e = do arr <- readRef a
                                         unsafeWrite arr i e

    {-# INLINE writeArray #-}
    writeArray dyn@(Dynamic _ _ a) i e = do
        arr <- readRef a
        bounds <- getBounds arr
        if inRange bounds i
          then unsafeWrite arr (unsafeIndex bounds i) e
          else extendAndWrite dyn arr bounds i e

-- Helper function used to make `writeArray` look as non-recursive function,
-- what is required for GHC's optimization
extendAndWrite dyn@(Dynamic extend _ a) arr bounds i e = do
    resizeDynamicArray dyn (extend bounds i)
    writeArray dyn i e


-- ---------------------------------------------------------------------------
-- Bounds growing functions, that can be used with newDynamicArray/newDynamicArray_

-- | No automatic growing at all. This "growing" method is compatible with any
-- bounds type
noGrow _ _ = error "Dynamic array: index out of bounds"

-- | Grow minimally - only to include new index in array bounds. This growing
-- method is compatible with any bounds type
growMinimally (l,u) i | inRange (l,i) u = (l,i)
                      | inRange (i,u) l = (i,u)
                      | otherwise = error "can't compute new bounds for dynamic array"

-- | Grow number of elements at least 2 times. This growing method is compatible
-- only with bounds belonging to class Num
growTwoTimes (l,u) i = if i<l then (min (l-(u-l)) i, u)
                              else (l, max (u+(u-l)) i)


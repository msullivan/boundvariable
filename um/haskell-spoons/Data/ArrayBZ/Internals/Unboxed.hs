{-# OPTIONS_GHC -cpp -fglasgow-exts #-}
{- |
   Module     : Data.ArrayBZ.Internals.Unboxed
   Copyright  : (c) The University of Glasgow 2001 & (c) 2006 Bulat Ziganshin
   License    : BSD3

   Maintainer : Bulat Ziganshin <Bulat.Ziganshin@gmail.com>
   Stability  : experimental
   Portability: GHC/Hugs

Unboxed arrays

Based on the idea of Oleg Kiselyov
  (see http://www.haskell.org/pipermail/haskell-cafe/2004-July/006400.html)
-}

module Data.ArrayBZ.Internals.Unboxed where

import Control.Monad.ST         (ST, runST)
import Control.Monad.ST.Lazy    ( strictToLazyST )
import qualified Control.Monad.ST.Lazy as Lazy (ST)
import Data.Ix
import Data.Typeable
#include "Typeable.h"

import Control.Monad.STorIO
import Data.ArrayBZ.Internals.IArray
import Data.ArrayBZ.Internals.MArray
import Data.HasDefaultValue
import Data.Unboxed

-- ---------------------------------------------------------------------------
-- | Unboxed mutable arrays

data UnboxedMutableArray s i e  =  UMA !i !i !(MUVec s e)

instance HasBounds (UnboxedMutableArray s) where
    {-# INLINE bounds #-}
    bounds (UMA l u _) = (l,u)

instance (STorIO m s) => HasMutableBounds (UnboxedMutableArray s) m where
    {-# INLINE getBounds #-}
    getBounds (UMA l u _) = return (l,u)

instance (STorIO m s, Unboxed e) => MArray (UnboxedMutableArray s) e m where
    newArray_ (l,u) = do arr <- allocUnboxed (rangeSize (l,u))
                         return (UMA l u arr)
    {-# INLINE unsafeRead #-}
    unsafeRead  (UMA _ _ arr) index  =  readUnboxed  arr index
    {-# INLINE unsafeWrite #-}
    unsafeWrite (UMA _ _ arr) index  =  writeUnboxed arr index

-- ---------------------------------------------------------------------------
-- | Unboxed mutable arrays in ST monad

type STUArray = UnboxedMutableArray

INSTANCE_TYPEABLE3(STUArray,stUArrayTc,"STUArray")

-- ---------------------------------------------------------------------------
-- STUArray also works in Lazy ST monad

instance (Unboxed e) => MArray (STUArray s) e (Lazy.ST s) where
    {-# INLINE newArray_ #-}
    newArray_   (l,u)   = strictToLazyST (newArray_ (l,u))
    {-# INLINE unsafeRead #-}
    unsafeRead  arr i   = strictToLazyST (unsafeRead  arr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite arr i e = strictToLazyST (unsafeWrite arr i e)

-- ---------------------------------------------------------------------------
-- | Unboxed mutable arrays in IO monad

type IOUArray = IOSpecific3 UnboxedMutableArray

INSTANCE_TYPEABLE2(IOUArray,iOUArrayTc,"IOUArray")

-- ---------------------------------------------------------------------------
-- | Unboxed arrays

data UArray i e  =  UA !i !i !(UVec e)

INSTANCE_TYPEABLE2(UArray,uArrayTc,"UArray")

instance HasBounds UArray where
    {-# INLINE bounds #-}
    bounds (UA l u _) = (l,u)

instance (Unboxed e, HasDefaultValue e) => IArray UArray e where
    {-# INLINE unsafeArray #-}
    -- Create new array filled with (i,e) values
    unsafeArray lu ies = runST (withNewArray lu defaultValue (doReplace ies))
    {-# INLINE unsafeAt #-}
    unsafeAt (UA _ _ arr) index = indexUnboxed arr index
    {-# INLINE unsafeReplace #-}
    -- Make a copy of array and perform (i,e) replacements
    unsafeReplace arr ies = runST (withArrayCopy arr (doReplace ies))
    {-# INLINE unsafeAccum #-}
    -- Make a copy of array and perform (i,e) accumulation in new array
    unsafeAccum f arr ies = runST (withArrayCopy arr (doAccum f ies))
    {-# INLINE unsafeAccumArray #-}
    -- Create new array accumulating (i,e) values
    unsafeAccumArray f init lu ies = runST (withNewArray lu init (doAccum f ies))


-- Implementation helper functions -------------

-- Create new array and perform given action on it before freezing
withNewArray lu init action = do
    marr <- newArray lu init
    action marr
    unsafeFreezeUA marr

-- Make a copy of array and perform given action on it before freezing
withArrayCopy arr action = do
    marr <- thawUA arr
    action marr
    unsafeFreezeUA marr

-- Perform (i,e) replaces in mutable array
doReplace ies marr = do
    sequence_ [unsafeWrite marr i e | (i, e) <- ies]

-- Accumulate (i,e) values in mutable array
doAccum f ies marr = do
    sequence_ [do old <- unsafeRead marr i
                  unsafeWrite marr i (f old new)
              | (i, new) <- ies]

-- Mutable->immutable array conversion which takes a copy of contents
freezeUA uma@(UMA l u marr) = do
    arr <- freezeUnboxed marr (sizeOfUMA uma)
    return (UA l u arr)

-- Immutable->mutable array conversion which takes a copy of contents
thawUA ua@(UA l u arr) = do
    marr <- thawUnboxed arr (sizeOfUA ua)
    return (UMA l u marr)

-- On-the-place mutable->immutable array conversion
unsafeFreezeUA (UMA l u marr) = do
    arr <- unsafeFreezeUnboxed marr
    return (UA l u arr)

-- On-the-place immutable->mutable array conversion
unsafeThawUA ua@(UA l u arr) = do
    marr <- unsafeThawUnboxed arr
    return (UMA l u marr)

-- | Array size in bytes
sizeOfUA  (arr :: UArray i e) =
     rangeSize (bounds arr) * sizeOfUnboxed (undefined::e)

sizeOfUMA (marr :: UnboxedMutableArray s i e) =
    rangeSize (bounds marr) * sizeOfUnboxed (undefined::e)

-- ---------------------------------------------------------------------------
-- | Freeze/thaw rules for IOUArray

freezeIOUArray       :: (Unboxed e, HasDefaultValue e, Ix i) => IOUArray i e -> IO (UArray i e)
thawIOUArray         :: (Unboxed e, HasDefaultValue e, Ix i) => UArray i e -> IO (IOUArray i e)
unsafeFreezeIOUArray :: (Unboxed e, HasDefaultValue e, Ix i) => IOUArray i e -> IO (UArray i e)
unsafeThawIOUArray   :: (Unboxed e, HasDefaultValue e, Ix i) => UArray i e -> IO (IOUArray i e)

freezeIOUArray       = freezeUA
thawIOUArray         = thawUA
unsafeFreezeIOUArray = unsafeFreezeUA
unsafeThawIOUArray   = unsafeThawUA

{-# RULES
"freeze/IOUArray" forall (x :: (forall s e i . (Unboxed e, HasDefaultValue e) => IOUArray i e)) . freeze x = freezeIOUArray x
"thaw/IOUArray"   forall (x :: (forall   e i . (Unboxed e, HasDefaultValue e) =>   UArray i e)) . thaw   x = thawIOUArray   x
"unsafeFreeze/IOUArray" forall (x :: (forall s e i . (Unboxed e, HasDefaultValue e) => IOUArray i e)) . unsafeFreeze x = unsafeFreezeIOUArray x
"unsafeThaw/IOUArray"   forall (x :: (forall   e i . (Unboxed e, HasDefaultValue e) =>   UArray i e)) . unsafeThaw   x = unsafeThawIOUArray   x
    #-}

-- ---------------------------------------------------------------------------
-- | Freeze/thaw rules for STUArray

freezeSTUArray       :: (Unboxed e, HasDefaultValue e, Ix i) => STUArray s i e -> ST s (UArray i e)
thawSTUArray         :: (Unboxed e, HasDefaultValue e, Ix i) => UArray i e -> ST s (STUArray s i e)
unsafeFreezeSTUArray :: (Unboxed e, HasDefaultValue e, Ix i) => STUArray s i e -> ST s (UArray i e)
unsafeThawSTUArray   :: (Unboxed e, HasDefaultValue e, Ix i) => UArray i e -> ST s (STUArray s i e)

freezeSTUArray       = freezeUA
thawSTUArray         = thawUA
unsafeFreezeSTUArray = unsafeFreezeUA
unsafeThawSTUArray   = unsafeThawUA

{-# RULES
"freeze/STUArray" forall (x :: (forall s e i . (Unboxed e, HasDefaultValue e) => STUArray s i e)) . freeze x = freezeSTUArray x
"thaw/STUArray"   forall (x :: (forall   e i . (Unboxed e, HasDefaultValue e) =>   UArray   i e)) . thaw   x = thawSTUArray   x
"unsafeFreeze/STUArray" forall (x :: (forall s e i . (Unboxed e, HasDefaultValue e) => STUArray s i e)) . unsafeFreeze x = unsafeFreezeSTUArray x
"unsafeThaw/STUArray"   forall (x :: (forall   e i . (Unboxed e, HasDefaultValue e) =>   UArray   i e)) . unsafeThaw   x = unsafeThawSTUArray   x
    #-}

-- ---------------------------------------------------------------------------
-- | Casts to arrays with different element type

-- | Casts an 'UArray' with one element type into 'UArray' with a
-- different element type. All the elements of the resulting array
-- are undefined (unless you know what you\'re doing...).
-- Upper array bound is recalculated according to elements size,
-- for example UArray (1,2) Word32 -> UArray (1,8) Word8
castUArray :: (Ix i, Enum i, Unboxed e, Unboxed e')
           => UArray i e -> UArray i e'
castUArray (UA l u vec :: UArray i e)  ::  UArray i e' =
    (UA l u' (castUnboxed vec))
        where u' = toEnum (fromEnum l - 1 + newSize)
              newSize = rangeSize (l,u)   *   sizeOfUnboxed (undefined::e)
                                        `div` sizeOfUnboxed (undefined::e')

-- | Casts an 'IOUArray' with one element type into 'IOUArray' with a different
-- element type (upper bound is recalculated).
castIOUArray :: (Ix i, Enum i, Unboxed e, Unboxed e')
             => IOUArray i e -> IOUArray i e'
castIOUArray (UMA l u mvec :: IOUArray i e)  ::  IOUArray i e' =
    (UMA l u' (castMUnboxed mvec))
        where u' = toEnum (fromEnum l - 1 + newSize)
              newSize = rangeSize (l,u)   *   sizeOfUnboxed (undefined::e)
                                        `div` sizeOfUnboxed (undefined::e')

-- | Casts an 'STUArray' with one element type into 'STUArray' with a different
-- element type (upper bound is recalculated).
castSTUArray :: (Ix i, Enum i, Unboxed e, Unboxed e')
             => STUArray s i e -> STUArray s i e'
castSTUArray (UMA l u mvec :: STUArray s i e)  ::  STUArray s i e' =
    (UMA l u' (castMUnboxed mvec))
        where u' = toEnum (fromEnum l - 1 + newSize)
              newSize = rangeSize (l,u)   *   sizeOfUnboxed (undefined::e)
                                        `div` sizeOfUnboxed (undefined::e')

-- ---------------------------------------------------------------------------
-- | Instances

instance (Ix i, Show i, Show e, Unboxed e, HasDefaultValue e) => Show (UArray i e) where
    showsPrec = showsIArray

instance (Ix i, Eq i, Eq e, Unboxed e, HasDefaultValue e) => Eq (UArray i e) where
    (==) = eqIArray

instance (Ix i, Ord i, Ord e, Unboxed e, HasDefaultValue e) => Ord (UArray i e) where
    compare = cmpIArray


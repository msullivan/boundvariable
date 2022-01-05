{-# OPTIONS_GHC -cpp -fglasgow-exts #-}
{- |
   Module     : Data.ArrayBZ.Internals.Boxed
   Copyright  : (c) The University of Glasgow 2001 & (c) 2006 Bulat Ziganshin
   License    : BSD3

   Maintainer : Bulat Ziganshin <Bulat.Ziganshin@gmail.com>
   Stability  : experimental
   Portability: GHC/Hugs

Boxed arrays

-}

module Data.ArrayBZ.Internals.Boxed
#ifdef __HUGS__
         ( -- * Types
           Array,
           IOArray,
           STArray,

           -- * Freeze/thaw operations
           freezeIOArray,
           thawIOArray,
           unsafeFreezeIOArray,
           freezeSTArray,
           thawSTArray,
           unsafeFreezeSTArray,
         )
#endif
   where

import Control.Monad.ST         ( ST, runST )
import Control.Monad.ST.Lazy    ( strictToLazyST )
import qualified Control.Monad.ST.Lazy as Lazy (ST)
import Data.Ix
import Data.Typeable
#include "Typeable.h"

import Data.ArrayBZ.Internals.IArray
import Data.ArrayBZ.Internals.MArray

#ifdef __HUGS__
-- ---------------------------------------------------------------------------
-- Hugs primitives are higher-level than GHC/NHC's
-- ---------------------------------------------------------------------------

import Hugs.Array as Arr
import Hugs.IOArray
import Hugs.ST

-----------------------------------------------------------------------------
-- Normal polymorphic arrays

instance HasBounds Array where
    bounds           = Arr.bounds

instance IArray Array e where
    unsafeArray      = Arr.unsafeArray
    unsafeAt         = Arr.unsafeAt
    unsafeReplace    = Arr.unsafeReplace
    unsafeAccum      = Arr.unsafeAccum
    unsafeAccumArray = Arr.unsafeAccumArray

-----------------------------------------------------------------------------
-- | Instance declarations for 'IOArray's

instance HasBounds IOArray where
    bounds      = boundsIOArray

instance HasMutableBounds IOArray IO where
    getBounds   = return . boundsIOArray

instance MArray IOArray e IO where
    newArray    = newIOArray
    unsafeRead  = unsafeReadIOArray
    unsafeWrite = unsafeWriteIOArray

-----------------------------------------------------------------------------
-- Polymorphic non-strict mutable arrays (ST monad)

instance HasBounds (STArray s) where
    bounds      = boundsSTArray

instance HasMutableBounds (STArray s) (ST s) where
    getBounds   = return . boundsSTArray

instance MArray (STArray s) e (ST s) where
    newArray    = newSTArray
    unsafeRead  = unsafeReadSTArray
    unsafeWrite = unsafeWriteSTArray

#else
-- ---------------------------------------------------------------------------
-- Non-Hugs implementation
-- ---------------------------------------------------------------------------

import Control.Monad.STorIO
import GHC.ArrBZ

-- ---------------------------------------------------------------------------
-- | Boxed mutable arrays

data BoxedMutableArray s i e  =  BMA !i !i !(MVec s e)

instance HasBounds (BoxedMutableArray s) where
    {-# INLINE bounds #-}
    bounds (BMA l u _) = (l,u)

instance (STorIO m s) => HasMutableBounds (BoxedMutableArray s) m where
    {-# INLINE getBounds #-}
    getBounds (BMA l u _) = return (l,u)

instance (STorIO m s) => MArray (BoxedMutableArray s) e m where
    newArray (l,u) init = do arr <- allocBoxed (rangeSize (l,u)) init
                             return (BMA l u arr)
    {-# INLINE unsafeRead #-}
    unsafeRead  (BMA _ _ arr) index  =  readBoxed  arr index
    {-# INLINE unsafeWrite #-}
    unsafeWrite (BMA _ _ arr) index  =  writeBoxed arr index

-- ---------------------------------------------------------------------------
-- | Boxed mutable arrays in ST monad

type STArray = BoxedMutableArray

-- ---------------------------------------------------------------------------
-- STArray also works in Lazy ST monad

instance MArray (STArray s) e (Lazy.ST s) where
    {-# INLINE newArray #-}
    newArray (l,u) init = strictToLazyST (newArray (l,u) init)
    {-# INLINE unsafeRead #-}
    unsafeRead  arr i   = strictToLazyST (unsafeRead  arr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite arr i e = strictToLazyST (unsafeWrite arr i e)

-- ---------------------------------------------------------------------------
-- | Boxed mutable arrays in IO monad

type IOArray = IOSpecific3 BoxedMutableArray

-- ---------------------------------------------------------------------------
-- | Boxed immutable arrays

data Array i e  =  BA !i !i !(Vec e)

instance HasBounds Array where
    {-# INLINE bounds #-}
    bounds (BA l u _) = (l,u)

instance IArray Array e where
    {-# INLINE unsafeArray #-}
    -- Create new array filled with (i,e) values
    unsafeArray lu ies = runST (withNewArray lu arrEleBottom (doReplace ies))
    {-# INLINE unsafeAt #-}
    unsafeAt (BA _ _ arr) index = indexBoxed arr index
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
    unsafeFreezeBA marr

-- Make a copy of array and perform given action on it before freezing
withArrayCopy arr action = do
    marr <- thawBA arr
    action marr
    unsafeFreezeBA marr

-- Perform (i,e) replaces in mutable array
doReplace ies marr = do
    sequence_ [unsafeWrite marr i e | (i, e) <- ies]

-- Accumulate (i,e) values in mutable array
doAccum f ies marr = do
    sequence_ [do old <- unsafeRead marr i
                  unsafeWrite marr i (f old new)
              | (i, new) <- ies]

-- Mutable->immutable array conversion which takes a copy of contents
freezeBA ua@(BMA l u marr) = do
    arr <- freezeBoxed marr (sizeOfBA ua) arrEleBottom
    return (BA l u arr)

-- Immutable->mutable array conversion which takes a copy of contents
thawBA ua@(BA l u arr) = do
    marr <- thawBoxed arr (sizeOfBA ua) arrEleBottom
    return (BMA l u marr)

-- On-the-place mutable->immutable array conversion
unsafeFreezeBA (BMA l u marr) = do
    arr <- unsafeFreezeBoxed marr
    return (BA l u arr)

-- On-the-place immutable->mutable array conversion
unsafeThawBA ua@(BA l u arr) = do
    marr <- unsafeThawBoxed arr
    return (BMA l u marr)

-- | Number of array elements
sizeOfBA arr = rangeSize (bounds arr)

-- ---------------------------------------------------------------------------
-- | Freeze/thaw rules for IOArray

freezeIOArray       :: (Ix i) => IOArray i e -> IO (Array i e)
thawIOArray         :: (Ix i) => Array i e -> IO (IOArray i e)
unsafeFreezeIOArray :: (Ix i) => IOArray i e -> IO (Array i e)
unsafeThawIOArray   :: (Ix i) => Array i e -> IO (IOArray i e)

freezeIOArray       = freezeBA
thawIOArray         = thawBA
unsafeFreezeIOArray = unsafeFreezeBA
unsafeThawIOArray   = unsafeThawBA

{-# RULES
"freeze/IOArray" freeze = freezeIOArray
"thaw/IOArray"   thaw   = thawIOArray
"unsafeFreeze/IOArray" unsafeFreeze = unsafeFreezeIOArray
"unsafeThaw/IOArray"   unsafeThaw   = unsafeThawIOArray
    #-}

-- ---------------------------------------------------------------------------
-- | Freeze/thaw rules for STArray

freezeSTArray       :: (Ix i) => STArray s i e -> ST s (Array i e)
thawSTArray         :: (Ix i) => Array i e -> ST s (STArray s i e)
unsafeFreezeSTArray :: (Ix i) => STArray s i e -> ST s (Array i e)
unsafeThawSTArray   :: (Ix i) => Array i e -> ST s (STArray s i e)

freezeSTArray       = freezeBA
thawSTArray         = thawBA
unsafeFreezeSTArray = unsafeFreezeBA
unsafeThawSTArray   = unsafeThawBA

{-# RULES
"freeze/STArray" freeze = freezeSTArray
"thaw/STArray"   thaw   = thawSTArray
"unsafeFreeze/STArray" unsafeFreeze = unsafeFreezeSTArray
"unsafeThaw/STArray"   unsafeThaw   = unsafeThawSTArray
    #-}

-- ---------------------------------------------------------------------------
-- | Instances

instance (Ix i, Show i, Show e) => Show (Array i e) where
    showsPrec = showsIArray

instance (Ix i, Eq i, Eq e) => Eq (Array i e) where
    (==) = eqIArray

instance (Ix i, Ord i, Ord e) => Ord (Array i e) where
    compare = cmpIArray

#endif


INSTANCE_TYPEABLE2(IOArray,iOArrayTc,"IOArray")

INSTANCE_TYPEABLE3(STArray,stArrayTc,"STArray")


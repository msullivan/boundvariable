{-# OPTIONS_GHC -cpp -fglasgow-exts #-}
{- |
   Module     : Data.ArrayBZ.MArray
   Copyright  : (c) The University of Glasgow 2001 & (c) 2006 Bulat Ziganshin
   License    : BSD3

   Maintainer : Bulat Ziganshin <Bulat.Ziganshin@gmail.com>
   Stability  : experimental
   Portability: Hugs/GHC

An overloaded interface to mutable arrays.  For array types which can be
used with this interface, see "Data.ArrayBZ.IO", "Data.ArrayBZ.ST",
and "Data.ArrayBZ.Storable".

-}

module Data.ArrayBZ.MArray (
    -- * Class of mutable array types
    MArray,       -- :: (* -> * -> *) -> * -> (* -> *) -> class

    -- * Class of array types with bounds
    HasBounds,    -- :: (* -> * -> *) -> class

    -- * Class of array types with mutable bounds
    HasMutableBounds,    -- :: (* -> * -> *) -> (* -> *) -> class

    -- * The @Ix@ class and operations
    module Data.Ix,

    -- * Constructing mutable arrays
    newArray,     -- :: (MArray a e m, Ix i) => (i,i) -> e -> m (a i e)
    newArray_,    -- :: (MArray a e m, Ix i) => (i,i) -> m (a i e)
    newListArray, -- :: (MArray a e m, Ix i) => (i,i) -> [e] -> m (a i e)

    -- * Reading and writing mutable arrays
    readArray,    -- :: (MArray a e m, Ix i) => a i e -> i -> m e
    writeArray,   -- :: (MArray a e m, Ix i) => a i e -> i -> e -> m ()

    -- * Derived arrays
    mapArray,     -- :: (MArray a e' m, MArray a e m, Ix i) => (e' -> e) -> a i e' -> m (a i e)
    mapIndices,   -- :: (MArray a e m, Ix i, Ix j) => (i,i) -> (i -> j) -> a j e -> m (a i e)

    -- * Deconstructing mutable arrays
    bounds,       -- :: (HasBounds a, Ix i) => a i e -> (i,i)
    indices,      -- :: (HasBounds a, Ix i) => a i e -> [i]
    getBounds,    -- :: (HasMutableBounds a, Ix i) => a i e -> m (i,i)
    getIndices,   -- :: (HasMutableBounds a, Ix i) => a i e -> m [i]
    getElems,     -- :: (MArray a e m, Ix i) => a i e -> m [e]
    getAssocs,    -- :: (MArray a e m, Ix i) => a i e -> m [(i, e)]

    -- * Conversions between mutable and immutable arrays
    freeze,       -- :: (Ix i, MArray a e m, IArray b e) => a i e -> m (b i e)
    unsafeFreeze, -- :: (Ix i, MArray a e m, IArray b e) => a i e -> m (b i e)
    thaw,         -- :: (Ix i, IArray a e, MArray b e m) => a i e -> m (b i e)
    unsafeThaw,   -- :: (Ix i, IArray a e, MArray b e m) => a i e -> m (b i e)
  ) where

import Data.Ix

import Data.ArrayBZ.Internals.IArray
import Data.ArrayBZ.Internals.MArray

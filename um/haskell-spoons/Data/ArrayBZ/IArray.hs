{-# OPTIONS_GHC -cpp -fglasgow-exts #-}
{- |
   Module     : Data.ArrayBZ.IArray
   Copyright  : (c) The University of Glasgow 2001 & (c) 2006 Bulat Ziganshin
   License    : BSD3

   Maintainer : Bulat Ziganshin <Bulat.Ziganshin@gmail.com>
   Stability  : experimental
   Portability: Hugs/GHC

Immutable arrays, with an overloaded interface.  For array types which
can be used with this interface, see the 'Array' type exported by this
module, and the "Data.ArrayBZ.Unboxed" and "Data.ArrayBZ.Diff" modules.

-}

module Data.ArrayBZ.IArray (
    -- * Array classes
    HasBounds,  -- :: (* -> * -> *) -> class
    IArray,     -- :: (* -> * -> *) -> * -> class

    module Data.Ix,

    -- * Immutable non-strict (boxed) arrays
    Array,

    -- * Array construction
    array,      -- :: (IArray a e, Ix i) => (i,i) -> [(i, e)] -> a i e
    listArray,  -- :: (IArray a e, Ix i) => (i,i) -> [e] -> a i e
    accumArray, -- :: (IArray a e, Ix i) => (e -> e' -> e) -> e -> (i,i) -> [(i, e')] -> a i e

    -- * Accessing arrays
    (!),        -- :: (IArray a e, Ix i) => a i e -> i -> e
    bounds,     -- :: (HasBounds a, Ix i) => a i e -> (i,i)
    indices,    -- :: (HasBounds a, Ix i) => a i e -> [i]
    elems,      -- :: (IArray a e, Ix i) => a i e -> [e]
    assocs,     -- :: (IArray a e, Ix i) => a i e -> [(i, e)]

    -- * Incremental array updates
    (//),       -- :: (IArray a e, Ix i) => a i e -> [(i, e)] -> a i e
    accum,      -- :: (IArray a e, Ix i) => (e -> e' -> e) -> a i e -> [(i, e')] -> a i e

    -- * Derived arrays
    amap,       -- :: (IArray a e', IArray a e, Ix i) => (e' -> e) -> a i e' -> a i e
    ixmap,      -- :: (IArray a e, Ix i, Ix j) => (i,i) -> (i -> j) -> a j e -> a i e
 )  where

import Data.Ix
import Data.ArrayBZ.Internals.IArray
import Data.ArrayBZ.Internals.Boxed (Array)

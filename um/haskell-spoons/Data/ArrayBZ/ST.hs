{-# OPTIONS_GHC -cpp -fglasgow-exts #-}
{- |
   Module     : Data.ArrayBZ.ST
   Copyright  : (c) The University of Glasgow 2001 & (c) 2006 Bulat Ziganshin
   License    : BSD3

   Maintainer : Bulat Ziganshin <Bulat.Ziganshin@gmail.com>
   Stability  : experimental
   Portability: Hugs/GHC

Mutable boxed and unboxed arrays in the 'Control.Monad.ST.ST' monad.

-}

module Data.ArrayBZ.ST (

   -- * Boxed arrays
   STArray,             -- instance of: Eq, MArray
   runSTArray,

   -- * Unboxed arrays
   STUArray,            -- instance of: Eq, MArray
   runSTUArray,
   castSTUArray,        -- :: STUArray s i a -> ST s (STUArray s i b)

   -- * Overloaded mutable array interface
   module Data.ArrayBZ.MArray,
 ) where

import Control.Monad.ST ( ST, runST )

import Data.HasDefaultValue
import Data.Unboxed
import Data.ArrayBZ.MArray
import Data.ArrayBZ.Internals.Boxed
import Data.ArrayBZ.Internals.Unboxed

-- | A safe way to create and work with a mutable array before returning an
-- immutable array for later perusal.  This function avoids copying
-- the array before returning it - it uses 'unsafeFreeze' internally, but
-- this wrapper is a safe interface to that function.
--
runSTArray :: (Ix i)
           => (forall s . ST s (STArray s i e))
           -> Array i e
runSTArray st = runST (st >>= unsafeFreezeSTArray)

-- | A safe way to create and work with an unboxed mutable array before
-- returning an immutable array for later perusal.  This function
-- avoids copying the array before returning it - it uses
-- 'unsafeFreeze' internally, but this wrapper is a safe interface to
-- that function.
--
runSTUArray :: (Unboxed e, HasDefaultValue e, Ix i)
           => (forall s . ST s (STUArray s i e))
           -> UArray i e
runSTUArray st = runST (st >>= unsafeFreezeSTUArray)


{- |
   Module     : Data.ArrayBZ.Unboxed
   Copyright  : (c) The University of Glasgow 2001 & (c) 2006 Bulat Ziganshin
   License    : BSD3

   Maintainer : Bulat Ziganshin <Bulat.Ziganshin@gmail.com>
   Stability  : experimental
   Portability: Hugs/GHC

Unboxed arrays

-}

module Data.ArrayBZ.Unboxed (
           UArray,
           IOUArray,
           STUArray,
           castUArray,
           castIOUArray,
           castSTUArray,
           module Data.ArrayBZ.Internals.IArray,
           module Data.ArrayBZ.Internals.MArray,
  ) where

import Data.ArrayBZ.Internals.Unboxed
import Data.ArrayBZ.Internals.IArray
import Data.ArrayBZ.Internals.MArray

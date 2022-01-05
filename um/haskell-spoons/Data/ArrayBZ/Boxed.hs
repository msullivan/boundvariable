{- |
   Module     : Data.ArrayBZ.Boxed
   Copyright  : (c) The University of Glasgow 2001 & (c) 2006 Bulat Ziganshin
   License    : BSD3

   Maintainer : Bulat Ziganshin <Bulat.Ziganshin@gmail.com>
   Stability  : experimental
   Portability: Hugs/GHC

Boxed arrays

-}

module Data.ArrayBZ.Boxed (
           Array,
           IOArray,
           STArray,
           module Data.Ix,
           module Data.ArrayBZ.Internals.IArray,
           module Data.ArrayBZ.Internals.MArray,
  ) where

import Data.Ix
import Data.ArrayBZ.Internals.Boxed
import Data.ArrayBZ.Internals.IArray
import Data.ArrayBZ.Internals.MArray

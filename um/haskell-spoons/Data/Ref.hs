{- |
   Module     : Data.Ref
   Copyright  : Copyright (C) 2006 Bulat Ziganshin
   License    : BSD3

   Maintainer : Bulat Ziganshin <Bulat.Ziganshin@gmail.com>
   Stability  : experimental
   Portability: Hugs/GHC

References (mutable vars)

-}

module Data.Ref (
    module Data.IORef,
    module Data.STRef,
    module Data.Ref.Unboxed,
    module Data.Ref.Universal,
  ) where

import Data.IORef
import Data.STRef
import Data.Ref.Unboxed
import Data.Ref.Universal

{-# OPTIONS_GHC -cpp -fglasgow-exts #-}
{- |
   Module     : Control.Monad.STorIO
   Copyright  : Copyright (C) 2006 Bulat Ziganshin
   License    : BSD3

   Maintainer : Bulat Ziganshin <Bulat.Ziganshin@gmail.com>
   Stability  : experimental
   Portability: GHC/Hugs

Unification of ST and IO operations!

-}

module Control.Monad.STorIO (
           STorIO(..),
           IOSpecific,
           IOSpecific2,
           IOSpecific3,
       )
where

#ifdef __GLASGOW_HASKELL__

import GHC.Unboxed

#else

import Control.Monad.ST
import qualified Control.Monad.ST.Lazy as Lazy

-- ---------------------------------------------------------------------------
-- | That's all we need to unify ST and IO operations!

class (Monad m) => STorIO m s | m->s where
    mLift :: IO a -> m a

instance STorIO (ST s) s where
    {-# INLINE mLift #-}
    mLift = unsafeIOToST

instance STorIO (Lazy.ST s) s where
    {-# INLINE mLift #-}
    mLift = Lazy.unsafeIOToST

instance STorIO IO () where
    {-# INLINE mLift #-}
    mLift = id

-- | Type functions which converts universal ST/IO types to IO-specific ones
type IOSpecific  a = a ()
type IOSpecific2 a b = a () b
type IOSpecific3 a = a ()

#endif


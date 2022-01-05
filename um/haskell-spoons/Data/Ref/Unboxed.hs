{-# OPTIONS_GHC -cpp -fglasgow-exts #-}
{- |
   Module     : Data.Ref.Unboxed
   Copyright  : Copyright (C) 2006 Bulat Ziganshin
   License    : BSD3

   Maintainer : Bulat Ziganshin <Bulat.Ziganshin@gmail.com>
   Stability  : experimental
   Portability: GHC/Hugs

Unboxed references

Based on the idea of Oleg Kiselyov
  (see http://www.haskell.org/pipermail/haskell-cafe/2004-July/006400.html)
-}

module Data.Ref.Unboxed where

import Control.Monad.ST  (ST)
import Data.Typeable
#include "Typeable.h"

import Control.Monad.STorIO
import Data.Unboxed

-- -----------------------------------------------------------------------------
-- | Unboxed references in IO monad

newtype IOURef a = IOURef (IOSpecific2 MUVec a)

INSTANCE_TYPEABLE1(IOURef,ioURefTc,"IOURef")

-- | Create new unboxed reference in IO monad
newIOURef :: (Unboxed a) => a -> IO (IOURef a)
newIOURef init = do var <- allocUnboxed 1
                    writeUnboxed var 0 init
                    return (IOURef var)

-- | Read current value of unboxed reference in IO monad
{-# INLINE readIOURef #-}
readIOURef :: (Unboxed a) => IOURef a -> IO a
readIOURef (IOURef ref) = readUnboxed ref 0

-- | Change value of unboxed reference in IO monad
{-# INLINE writeIOURef #-}
writeIOURef :: (Unboxed a) => IOURef a -> a -> IO ()
writeIOURef (IOURef ref) = writeUnboxed ref 0

-- |Modify contents of an 'IOURef' by applying pure function to it
{-# INLINE modifyIOURef #-}
modifyIOURef :: (Unboxed a) => IOURef a -> (a -> a) -> IO ()
modifyIOURef ref f  =  readIOURef ref >>= writeIOURef ref . f

-- -----------------------------------------------------------------------------
-- | Unboxed references in ST monad

newtype STURef s a = STURef (MUVec s a)

INSTANCE_TYPEABLE2(STURef,stURefTc,"STURef")

-- | Create new unboxed reference in ST monad
newSTURef :: (Unboxed a) => a -> ST s (STURef s a)
newSTURef init = do var <- allocUnboxed 1
                    writeUnboxed var 0 init
                    return (STURef var)

-- | Read current value of unboxed reference in ST monad
{-# INLINE readSTURef #-}
readSTURef :: (Unboxed a) => STURef s a -> ST s a
readSTURef (STURef ref) = readUnboxed ref 0

-- | Change value of unboxed reference in ST monad
{-# INLINE writeSTURef #-}
writeSTURef :: (Unboxed a) => STURef s a -> a -> ST s ()
writeSTURef (STURef ref) = writeUnboxed ref 0

-- |Modify contents of an 'STURef' by applying pure function to it
{-# INLINE modifySTURef #-}
modifySTURef :: (Unboxed a) => STURef s a -> (a -> a) -> ST s ()
modifySTURef ref f  =  readSTURef ref >>= writeSTURef ref . f


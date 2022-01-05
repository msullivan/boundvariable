{-# OPTIONS_GHC -cpp -fglasgow-exts #-}
{- |
   Module     : Data.Ref.Universal
   Copyright  : Copyright (C) 2006 Bulat Ziganshin
   License    : BSD3

   Maintainer : Bulat Ziganshin <Bulat.Ziganshin@gmail.com>
   Stability  : experimental
   Portability: Hugs/GHC

Monad-independent interfaces for boxed and unboxed references

-}

module Data.Ref.Universal (
           -- * Monad-independent interface for boxed references
           Ref(..),
           modifyRef,
           modifyRefM,
           -- * Monad-independent interface for unboxed references
           URef(..),
           modifyURef,
           modifyURefM,
       )
where

import Control.Monad.ST (ST)
import Data.IORef
import Data.STRef

import Data.Unboxed
import Data.Ref.Unboxed

-- -----------------------------------------------------------------------------
-- | This class allows to create new boxed reference in monad-independent way
--     (suitable for writing code that will work in IO, ST and other monads)

class (Monad m) => Ref m r | m->r, r->m where
    -- |Create a new 'Ref' with given initial value
    newRef :: a -> m (r a)
    -- |Read the value of an 'Ref'
    readRef   :: r a -> m a
    -- |Write new value into an 'Ref'
    writeRef  :: r a -> a -> m ()

instance Ref IO IORef where
    newRef = newIORef
    readRef = readIORef
    writeRef = writeIORef
instance Ref (ST s) (STRef s) where
    newRef = newSTRef
    readRef = readSTRef
    writeRef = writeSTRef

-- |Modify the contents of an 'Ref' by applying pure function to it
{-# INLINE modifyRef #-}
modifyRef  ref f  =  readRef ref >>= writeRef ref . f

-- |Modify the contents of an 'Ref' by applying monadic computation to it
{-# INLINE modifyRefM #-}
modifyRefM ref f  =  readRef ref >>= f >>= writeRef ref

-- -----------------------------------------------------------------------------
-- | This class allows to create new unboxed reference in monad-independent way
--     (suitable for writing code that will work in IO, ST and other monads)

class (Monad m) => URef m r | m->r, r->m where
    -- |Create a new 'URef' with given initial value
    newURef    :: (Unboxed a) => a -> m (r a)
    -- |Read the value of an 'URef'
    readURef   :: (Unboxed a) => r a -> m a
    -- |Write new value into an 'URef'
    writeURef  :: (Unboxed a) => r a -> a -> m ()

instance URef IO IOURef where
    newURef = newIOURef
    readURef = readIOURef
    writeURef = writeIOURef
instance URef (ST s) (STURef s) where
    newURef = newSTURef
    readURef = readSTURef
    writeURef = writeSTURef

-- |Modify the contents of an 'URef' by applying pure function to it
{-# INLINE modifyURef #-}
modifyURef  ref f  =  readURef ref >>= writeURef ref . f

-- |Modify the contents of an 'URef' by applying monadic computation to it
{-# INLINE modifyURefM #-}
modifyURefM ref f  =  readURef ref >>= f >>= writeURef ref


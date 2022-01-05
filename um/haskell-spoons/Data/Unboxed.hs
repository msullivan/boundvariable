{-# OPTIONS_GHC -cpp -fglasgow-exts #-}
{- |
   Module     : Data.Unboxed
   Copyright  : Copyright (C) 2006 Bulat Ziganshin
   License    : BSD3

   Maintainer : Bulat Ziganshin <Bulat.Ziganshin@gmail.com>
   Stability  : experimental
   Portability: Hugs/GHC

Class 'Unboxed' represents values that can be stored in unboxed vectors
  and unboxed references

Based on the: Hugs.ByteArray module
-}

module Data.Unboxed (
           UVec,
           MUVec,
           allocUnboxed,
           unsafeFreezeUnboxed,
           unsafeThawUnboxed,
           freezeUnboxed,
           thawUnboxed,
           castUnboxed,
           castMUnboxed,

           Unboxed,
           readUnboxed,
           writeUnboxed,
           indexUnboxed,
           sizeOfUnboxed,
       )
where

-- On GHC we use fast compiler-specific implementation.
-- On other compilers, slow but universal Storable-based implementation is used
#ifdef __GLASGOW_HASKELL__

import GHC.Unboxed

#else

import Control.Monad.STorIO
import Data.Int
import Data.Word
import Foreign.ForeignPtr
import Foreign.Marshal.Utils    ( copyBytes )
import Foreign.Ptr
import Foreign.StablePtr
import Foreign.Storable
import System.IO.Unsafe

-- ---------------------------------------------------------------------------
-- | Immutable and mutable byte vectors

newtype  UVec   a =  UVec (ForeignPtr a)
newtype MUVec s a = MUVec (ForeignPtr a)

-- | Alloc the mutable byte vector
allocUnboxedBytes :: (STorIO m s, Integral bytes, Unboxed a)
                  => bytes -> m (MUVec s a)
allocUnboxedBytes bytes = do
    fp <- mLift (mallocForeignPtrBytes (fromIntegral bytes))
    return (MUVec fp)

-- | Mutable->immutable byte vector on-place conversion
{-# INLINE unsafeFreezeMUVec #-}
unsafeFreezeUnboxed :: (STorIO m s)
                => MUVec s a -> m (UVec a)
unsafeFreezeUnboxed (MUVec arr) = do
    return (UVec arr)

-- | Immutable->mutable byte vector on-place conversion
{-# INLINE unsafeThawUVec #-}
unsafeThawUnboxed :: (STorIO m s)
             => UVec a -> m (MUVec s a)
unsafeThawUnboxed (UVec arr) = do
    return (MUVec arr)

-- | Mutable->immutable byte vector conversion which takes a copy of contents
freezeUnboxed :: (STorIO m s)
                => MUVec s a -> Int -> m (UVec a)
freezeUnboxed (MUVec arr) size = mLift $ do
    arr' <- mallocForeignPtrBytes size
    withForeignPtr arr $ \p ->
        withForeignPtr arr' $ \p' ->
            copyBytes p' p size
    return (UVec arr')

-- | Immutable->mutable byte vector conversion which takes a copy of contents
thawUnboxed :: (STorIO m s) => UVec a -> Int -> m (MUVec s a)
thawUnboxed (UVec arr) size = mLift $ do
    arr' <- mallocForeignPtrBytes size
    withForeignPtr arr $ \p ->
        withForeignPtr arr' $ \p' ->
            copyBytes p' p size
    return (MUVec arr')

-- ---------------------------------------------------------------------------
-- | Unboxed defined via Storable

class    (Storable value) => Unboxed value
instance (Storable value) => Unboxed value

-- | Read the value from mutable byte vector at given `index`
{-# INLINE readUnboxed #-}
readUnboxed :: (STorIO m s, Unboxed value, Integral index)
            => MUVec s value -> index -> m value
readUnboxed (MUVec arr) index = mLift $
    withForeignPtr arr $ \a -> peekElemOff a (fromIntegral index)

-- | Write the value to mutable byte vector at given `index`
{-# INLINE writeUnboxed #-}
writeUnboxed :: (STorIO m s, Unboxed value, Integral index)
             => MUVec s value -> index -> value -> m ()
writeUnboxed (MUVec arr) index value = mLift $
    withForeignPtr arr $ \a -> pokeElemOff a (fromIntegral index) value

-- | Read the value from immutable byte vector at given `index`
{-# INLINE indexUnboxed #-}
indexUnboxed :: (Unboxed value, Integral index)
             => UVec value -> index -> value
indexUnboxed (UVec arr) index = unsafePerformIO $
    withForeignPtr arr $ \a -> peekElemOff a (fromIntegral index)

-- | How many bytes required to represent values of this type
{-# INLINE sizeOfUnboxed #-}
sizeOfUnboxed :: (Unboxed value, Integral size)
              => value -> size
sizeOfUnboxed = fromIntegral . sizeOf

-- | Recast immutable unboxed vector
castUnboxed :: UVec a    -> UVec b
castUnboxed   (UVec vec) =  UVec (castForeignPtr vec)

-- | Recast mutable unboxed vector
castMUnboxed :: MUVec s a   -> MUVec s b
castMUnboxed   (MUVec mvec) =  MUVec (castForeignPtr mvec)

#endif

-- ---------------------------------------------------------------------------
-- | Additional operations on byte vectors

-- | Alloc the mutable byte vector having `elems` elements of required type
allocUnboxed :: (STorIO m s, Integral elems, Unboxed a)
             => elems -> m (MUVec s a)
allocUnboxed elems :: m (MUVec s a) =
    allocUnboxedBytes (fromIntegral elems * sizeOfUnboxed (undefined::a))


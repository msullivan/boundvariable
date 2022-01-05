{-# OPTIONS_GHC -fglasgow-exts #-}
{- |
   Module     : Data.ArrayBZ.Storable
   Copyright  : (c) The University of Glasgow 2001 & (c) 2006 Bulat Ziganshin
   License    : BSD3

   Maintainer : Bulat Ziganshin <Bulat.Ziganshin@gmail.com>
   Stability  : experimental
   Portability: GHC/Hugs

A storable array is an IO-mutable array which stores its
contents in a contiguous memory block living in the C
heap. Elements are stored according to the class 'Storable'.
You can obtain the pointer to the array contents to manipulate
elements from languages like C.

It is similar to 'Data.ArrayBZ.IO.IOUArray' but slower.
Its advantage is that it's compatible with C.

-}

module Data.ArrayBZ.Storable (

    -- * Arrays of 'Storable' things.
    StorableArray, -- data StorableArray index element
                   --     -- index type must be in class Ix
                   --     -- element type must be in class Storable

    -- * Overloaded mutable array interface
    -- | Module "Data.ArrayBZ.Internals.MArray" provides the interface of storable arrays.
    -- They are instances of class 'MArray' (with the 'IO' monad).
    module Data.ArrayBZ.Internals.MArray,

    -- * Accessing the pointer to the array contents
    withStorableArray,  -- :: StorableArray i e -> (Ptr e -> IO a) -> IO a

    touchStorableArray, -- :: StorableArray i e -> IO ()

    -- * Casting ForeignPtr to StorableArray
    unsafeForeignPtrToStorableArray
    )
    where

import Data.Ix
import Foreign hiding (newArray)

import Data.ArrayBZ.Internals.IArray
import Data.ArrayBZ.Internals.MArray

-- ---------------------------------------------------------------------------

-- |The array type
data StorableArray i e = StorableArray !i !i !(ForeignPtr e)

instance HasBounds StorableArray where
    {-# INLINE bounds #-}
    bounds (StorableArray l u _) = (l,u)

instance HasMutableBounds StorableArray IO where
    {-# INLINE getBounds #-}
    getBounds (StorableArray l u _) = return (l,u)

instance Storable e => MArray StorableArray e IO where
    {-# INLINE newArray #-}
    newArray (l,u) init = do
        fp <- mallocForeignPtrArray size
        withForeignPtr fp $ \a ->
            sequence_ [pokeElemOff a i init | i <- [0..size-1]]
        return (StorableArray l u fp)
        where
        size = rangeSize (l,u)

    newArray_ (l,u) = do
        fp <- mallocForeignPtrArray (rangeSize (l,u))
        return (StorableArray l u fp)

    {-# INLINE unsafeRead #-}
    unsafeRead (StorableArray _ _ fp) i =
        withForeignPtr fp $ \a -> peekElemOff a i

    {-# INLINE unsafeWrite #-}
    unsafeWrite (StorableArray _ _ fp) i e =
        withForeignPtr fp $ \a -> pokeElemOff a i e


{-# INLINE withStorableArray #-}
-- |The pointer to the array contents is obtained by 'withStorableArray'.
-- The idea is similar to 'ForeignPtr' (used internally here).
-- The pointer should be used only during execution of the 'IO' action
-- retured by the function passed as argument to 'withStorableArray'.
withStorableArray :: StorableArray i e -> (Ptr e -> IO a) -> IO a
withStorableArray (StorableArray _ _ fp) f = withForeignPtr fp f

-- |If you want to use it afterwards, ensure that you
-- 'touchStorableArray' after the last use of the pointer,
-- so the array is not freed too early.
touchStorableArray :: StorableArray i e -> IO ()
touchStorableArray (StorableArray _ _ fp) = touchForeignPtr fp

-- |Construct a 'StorableArray' from an arbitrary 'ForeignPtr'.  It is
-- the caller's responsibility to ensure that the 'ForeignPtr' points to
-- an area of memory sufficient for the specified bounds.
unsafeForeignPtrToStorableArray
   :: ForeignPtr e -> (i,i) -> IO (StorableArray i e)
unsafeForeignPtrToStorableArray p (l,u) =
   return (StorableArray l u p)

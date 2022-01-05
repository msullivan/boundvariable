{-# OPTIONS_GHC -cpp -fglasgow-exts #-}
{- |
   Module     : GHC.Unboxed
   Copyright  : Copyright (C) 2006 Bulat Ziganshin
   License    : BSD3

   Maintainer : Bulat Ziganshin <Bulat.Ziganshin@gmail.com>
   Stability  : experimental
   Portability: GHC

Unboxed values (simple datatypes that can be stored in ByteArrays,
  i.e. raw memory buffers allocated inside the Haskell heap)

Based on the idea of Oleg Kiselyov
  (see http://www.haskell.org/pipermail/haskell-cafe/2004-July/006400.html)
-}

module GHC.Unboxed where

import GHC.ST           ( ST(..), runST )
import GHC.IOBase       ( IO(..) )
import GHC.Base
import GHC.Word         ( Word(..) )
import GHC.Ptr          ( Ptr(..), FunPtr(..) )
import GHC.Float        ( Float(..), Double(..) )
import GHC.Stable       ( StablePtr(..) )
import GHC.Int          ( Int8(..),  Int16(..),  Int32(..),  Int64(..) )
import GHC.Word         ( Word8(..), Word16(..), Word32(..), Word64(..) )
import Control.Monad.ST.Lazy    ( strictToLazyST )
import qualified Control.Monad.ST.Lazy as Lazy (ST)
import Foreign.Storable

-- ---------------------------------------------------------------------------
-- | That's all we need to unify ST and IO operations!

class (Monad m) => STorIO m s | m->s where
    mLift :: (State# s -> (# State# s, a #)) -> m a

instance STorIO (ST s) s where
    {-# INLINE mLift #-}
    mLift = ST

instance STorIO (Lazy.ST s) s where
    {-# INLINE mLift #-}
    mLift = strictToLazyST . ST

instance STorIO IO RealWorld where
    {-# INLINE mLift #-}
    mLift = IO

-- | Type functions which converts universal ST/IO types to IO-specific ones
type IOSpecific  (a :: * -> *)           = a RealWorld
type IOSpecific2 (a :: * -> * -> *)      = a RealWorld
type IOSpecific3 (a :: * -> * -> * -> *) = a RealWorld

-- ---------------------------------------------------------------------------
-- | Immutable and mutable byte vectors

data  UVec   a  =   UVec ByteArray#
data MUVec s a  =  MUVec (MutableByteArray# s)

-- | Alloc the mutable byte vector
allocUnboxedBytes :: (STorIO m s, Integral bytes, Unboxed a)
                  => bytes -> m (MUVec s a)
allocUnboxedBytes bytes = mLift ( \s ->
    case newByteArray# (fromI# bytes) s of { (# s, arr #) ->
    (# s, MUVec arr #) } )

-- | Mutable->immutable byte vector on-place conversion
{-# INLINE unsafeFreezeUnboxed #-}
unsafeFreezeUnboxed :: (STorIO m s) => MUVec s a -> m (UVec a)
unsafeFreezeUnboxed (MUVec marr#) = mLift ( \s ->
    case unsafeFreezeByteArray# marr# s of { (# s, arr# #) ->
    (# s, UVec arr# #) } )

-- | Immutable->mutable byte vector on-place conversion
{-# INLINE unsafeThawUnboxed #-}
unsafeThawUnboxed :: (STorIO m s) => UVec a -> m (MUVec s a)
unsafeThawUnboxed (UVec arr#) = mLift ( \s ->
    (# s, MUVec (unsafeCoerce# arr#) #) )

-- | Mutable->immutable byte vector conversion which takes a copy of contents
freezeUnboxed :: (STorIO m s) => MUVec s a -> Int -> m (UVec a)
freezeUnboxed (MUVec marr#) (I# size) = mLift ( \s1# ->
    case newByteArray# size                      s1# of { (# s2#, tmparr# #) ->
    case unsafeCoerce# memcpy tmparr# marr# size s2# of { (# s3#, () #) ->
    case unsafeFreezeByteArray# tmparr#          s3# of { (# s4#, arr# #) ->
    (# s3#, UVec arr# #) }}} )

-- | Immutable->mutable byte vector conversion which takes a copy of contents
thawUnboxed :: (STorIO m s) => UVec a -> Int -> m (MUVec s a)
thawUnboxed (UVec arr#) (I# size) = mLift (  \s1# ->
    case newByteArray# size                   s1# of { (# s2#, marr# #) ->
    case unsafeCoerce# memcpy marr# arr# size s2# of { (# s3#, () #) ->
    (# s3#, MUVec marr# #) }} )

-- | Recast immutable unboxed vector
castUnboxed :: UVec a    -> UVec b
castUnboxed   (UVec vec) =  UVec vec

-- | Recast mutable unboxed vector
castMUnboxed :: MUVec s a   -> MUVec s b
castMUnboxed   (MUVec mvec) =  MUVec mvec

-- Implementation helper function that converts any integral value to the Int#
{-# INLINE fromI# #-}
fromI# :: (Integral n) => n -> Int#
fromI# n = n#   where I# n# = fromIntegral n

-- Implementation helper function that copies data between byte vectors
foreign import ccall unsafe "memcpy"
    memcpy :: MutableByteArray# RealWorld -> ByteArray# -> Int# -> IO ()

-- ---------------------------------------------------------------------------
-- | Unboxed is like Storable, but values are stored in byte vectors (i.e. inside the Haskell heap)

class Unboxed value where
    -- | Read the value from mutable byte vector at given `index`
    readUnboxed    :: (STorIO m s, Integral index) => MUVec s value -> index -> m value
    -- | Write the value to mutable byte vector at given `index`
    writeUnboxed   :: (STorIO m s, Integral index) => MUVec s value -> index -> value -> m ()
    -- | Read the value from immutable byte vector at given `index`
    indexUnboxed   :: (Integral index) => UVec value -> index -> value
    -- | How many bytes required to represent values of this type
    sizeOfUnboxed  :: value -> Int

-- Universal defition for Enum types having <= 256 variants
instance Unboxed Bool where
{
    {-# INLINE readUnboxed #-};
    readUnboxed (MUVec arr) index  =  mLift ( \s ->
        case readInt8Array# arr (fromI# index) s of { (# s, value# #) ->
        (# s, tagToEnum# value# #) } );

    {-# INLINE writeUnboxed #-};
    writeUnboxed (MUVec arr) index value  =  mLift ( \s ->
        case writeInt8Array# arr (fromI# index) (getTag value) s of { s ->
        (# s, () #) } );

    {-# INLINE indexUnboxed #-};
    indexUnboxed (UVec arr) index  =  tagToEnum# (indexInt8Array# arr (fromI# index));

    {-# INLINE sizeOfUnboxed #-};
    sizeOfUnboxed _ = 1;
}

-- Universal defition for Storable types
#define InstanceUnboxed(type, cast, read, write, at)                  \
instance Unboxed type where                                           \
{                                                                     \
    {-# INLINE readUnboxed #-};                                       \
    readUnboxed (MUVec arr) index  =  mLift ( \s ->                   \
        case read arr (fromI# index) s of { (# s, value# #) ->        \
        (# s, cast value# #) } );                                     \
                                                                      \
    {-# INLINE writeUnboxed #-};                                      \
    writeUnboxed (MUVec arr) index (cast value#)  =  mLift ( \s ->    \
        case write arr (fromI# index) value# s of { s ->              \
        (# s, () #) } );                                              \
                                                                      \
    {-# INLINE indexUnboxed #-};                                      \
    indexUnboxed (UVec arr) index  =  cast (at arr (fromI# index));   \
                                                                      \
    {-# INLINE sizeOfUnboxed #-};                                     \
    sizeOfUnboxed = sizeOf;                                           \
}                                                                     \

InstanceUnboxed( Char,       C#,     readWideCharArray#, writeWideCharArray#, indexWideCharArray#)
InstanceUnboxed( Int,        I#,     readIntArray#,      writeIntArray#,      indexIntArray#)
InstanceUnboxed( Int8,       I8#,    readInt8Array#,     writeInt8Array#,     indexInt8Array#)
InstanceUnboxed( Int16,      I16#,   readInt16Array#,    writeInt16Array#,    indexInt16Array#)
InstanceUnboxed( Int32,      I32#,   readInt32Array#,    writeInt32Array#,    indexInt32Array#)
InstanceUnboxed( Int64,      I64#,   readInt64Array#,    writeInt64Array#,    indexInt64Array#)
InstanceUnboxed( Word,       W#,     readWordArray#,     writeWordArray#,     indexWordArray#)
InstanceUnboxed( Word8,      W8#,    readWord8Array#,    writeWord8Array#,    indexWord8Array#)
InstanceUnboxed( Word16,     W16#,   readWord16Array#,   writeWord16Array#,   indexWord16Array#)
InstanceUnboxed( Word32,     W32#,   readWord32Array#,   writeWord32Array#,   indexWord32Array#)
InstanceUnboxed( Word64,     W64#,   readWord64Array#,   writeWord64Array#,   indexWord64Array#)
InstanceUnboxed( Float,      F#,     readFloatArray#,    writeFloatArray#,    indexFloatArray#)
InstanceUnboxed( Double,     D#,     readDoubleArray#,   writeDoubleArray#,   indexDoubleArray#)
InstanceUnboxed( (Ptr a),    Ptr,    readAddrArray#,     writeAddrArray#,     indexAddrArray#)
InstanceUnboxed( (FunPtr a), FunPtr, readAddrArray#,     writeAddrArray#,     indexAddrArray#)
InstanceUnboxed( (StablePtr a), StablePtr, readStablePtrArray#, writeStablePtrArray#, indexStablePtrArray#)


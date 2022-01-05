{-# OPTIONS_GHC -cpp -fglasgow-exts #-}
{- |
   Module     : GHC.ArrBZ
   Copyright  : Copyright (C) 2006 Bulat Ziganshin
   License    : BSD3

   Maintainer : Bulat Ziganshin <Bulat.Ziganshin@gmail.com>
   Stability  : experimental
   Portability: GHC

Vectors of boxed values

-}

module GHC.ArrBZ where

import GHC.Base

import Control.Monad.STorIO

-- ---------------------------------------------------------------------------
-- | Immutable and mutable vectors of boxed values

data  Vec   a  =   Vec (Array# a)
data MVec s a  =  MVec (MutableArray# s a)

-- | Alloc the mutable vector
allocBoxed :: (STorIO m s, Integral elems) => elems -> a -> m (MVec s a)
-- | Mutable->immutable vector on-place conversion
unsafeFreezeBoxed :: (STorIO m s) => MVec s a -> m (Vec a)
-- | Immutable->mutable vector on-place conversion
unsafeThawBoxed :: (STorIO m s) => Vec a -> m (MVec s a)
-- | Mutable->immutable vector conversion which takes a copy of contents
freezeBoxed :: (STorIO m s) => MVec s a -> Int -> a -> m (Vec a)
-- | Immutable->mutable vector conversion which takes a copy of contents
thawBoxed :: (STorIO m s) => Vec a -> Int -> a -> m (MVec s a)


allocBoxed elems init = mLift ( \s ->
  case newArray# (fromI# elems) init s of { (# s, arr #) ->
  (# s, MVec arr #) } )

{-# INLINE unsafeFreezeBoxed #-}
unsafeFreezeBoxed (MVec mvec) = mLift ( \s ->
  case unsafeFreezeArray# mvec s of { (# s, vec #) ->
  (# s, Vec vec #) } )

{-# INLINE unsafeThawBoxed #-}
unsafeThawBoxed (Vec arr#) = mLift ( \s ->
  case unsafeThawArray# arr# s of { (# s, marr# #) ->
  (# s, MVec marr# #) } )

freezeBoxed (MVec marr#) (I# n#) init = mLift ( \s1# ->
    case newArray# n# init                s1#  of { (# s2#, tmparr# #) ->

    let copy i# s01# | i# ==# n# =        s01#
                     | otherwise =
            case readArray#  marr#   i#   s01# of { (# s02#, e #) ->
            case writeArray# tmparr# i# e s02# of { s03# ->
            copy (i# +# 1#)               s03# }} in

    case copy 0#                          s2#  of { s3# ->
    case unsafeFreezeArray# tmparr#       s3#  of { (# s4#, arr# #) ->
    (# s4#, Vec arr# #) }}} )

thawBoxed (Vec vec#) (I# n#) init = mLift ( \s1# ->
    case newArray# n# init s1#               of { (# s2#, mvec# #) ->

    let copy i# s01# | i# ==# n# =      s01#
                     | otherwise =
            case indexArray# vec#  i#        of { (# e #) ->
            case writeArray# mvec# i# e s01# of { s02# ->
            copy (i# +# 1#)             s02# }} in

    case copy 0#                        s2#  of { s3# ->
    (# s3#, MVec mvec# #) }} )

-- Implementation helper function that converts any integral value to the Int#
{-# INLINE fromI# #-}
fromI# :: (Integral n) => n -> Int#
fromI# n = n#   where I# n# = fromIntegral n

-- ---------------------------------------------------------------------------
-- Read/write/index vector elements

-- | Read the value from mutable vector at given `index`
readBoxed    :: (STorIO m s, Integral index) => MVec s value -> index -> m value
-- | Write the value to mutable vector at given `index`
writeBoxed   :: (STorIO m s, Integral index) => MVec s value -> index -> value -> m ()
-- | Read the value from immutable vector at given `index`
indexBoxed   :: (Integral index) => Vec value -> index -> value


{-# INLINE readBoxed #-}
readBoxed (MVec vec) index  =  mLift (readArray# vec (fromI# index))

{-# INLINE writeBoxed #-}
writeBoxed (MVec vec) index value  =  mLift ( \s ->
    case writeArray# vec (fromI# index) value s of { s ->
    (# s, () #) } )

{-# INLINE indexBoxed #-}
indexBoxed (Vec vec) index  =
    case indexArray# vec (fromI# index) of { (# s #) -> s }


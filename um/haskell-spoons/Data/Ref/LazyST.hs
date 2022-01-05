{-# OPTIONS_GHC -cpp -fglasgow-exts #-}
{- |
   Module     : Data.Ref.LazyST
   Copyright  : Copyright (C) 2006 Bulat Ziganshin
   License    : BSD3

   Maintainer : Bulat Ziganshin <Bulat.Ziganshin@gmail.com>
   Stability  : experimental
   Portability: GHC/Hugs

Mutable boxed and unboxed references in the lazy ST monad.

-}

module Data.Ref.LazyST (
        -- * STRefs
        STRef,          -- abstract, instance Eq
        newSTRef,       -- :: a -> ST s (STRef s a)
        readSTRef,      -- :: STRef s a -> ST s a
        writeSTRef,     -- :: STRef s a -> a -> ST s ()
        modifySTRef,    -- :: STRef s a -> (a -> a) -> ST s ()

        -- * STURefs
        ST.STURef,      -- abstract, instance Eq
        newSTURef,      -- :: a -> ST s (STURef s a)
        readSTURef,     -- :: STURef s a -> ST s a
        writeSTURef,    -- :: STURef s a -> a -> ST s ()
        modifySTURef    -- :: STURef s a -> (a -> a) -> ST s ()
 ) where

import Control.Monad.ST.Lazy
import Data.STRef.Lazy
import qualified Data.Ref.Unboxed as ST
import Data.Unboxed

newSTURef    :: (Unboxed a) => a -> ST s (ST.STURef s a)
readSTURef   :: (Unboxed a) => ST.STURef s a -> ST s a
writeSTURef  :: (Unboxed a) => ST.STURef s a -> a -> ST s ()
modifySTURef :: (Unboxed a) => ST.STURef s a -> (a -> a) -> ST s ()

newSTURef   = strictToLazyST . ST.newSTURef
readSTURef  = strictToLazyST . ST.readSTURef
writeSTURef r a = strictToLazyST (ST.writeSTURef r a)
modifySTURef r f = strictToLazyST (ST.modifySTURef r f)


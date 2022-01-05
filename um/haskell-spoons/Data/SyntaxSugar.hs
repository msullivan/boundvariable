{-# OPTIONS_GHC -cpp -fglasgow-exts #-}
{- |
   Module     : Data.SyntaxSugar
   Copyright  : Copyright (C) 2006 Bulat Ziganshin
   License    : BSD3

   Maintainer : Bulat Ziganshin <Bulat.Ziganshin@gmail.com>
   Stability  : experimental
   Portability: Hugs/GHC

Universal interface for reading and writing mutable data
  (references, array and hash elements)
Syntax sugar (=:, +=, val...) based on this interface

-}

module Data.SyntaxSugar where

import Control.Monad.ST (ST)
import Data.ArrayBZ.IO
import Data.ArrayBZ.ST
import Data.ArrayBZ.Storable
import Data.HashTable as Hash
import Data.Ref
import Data.Unboxed
import Foreign.Storable

-- -----------------------------------------------------------------------------
-- Universal interface for reading and writing mutable data
--   (references, array and hash elements)

class (Monad m) => Mutable m r a | r->a where
    -- |Read the value of an 'Mutable'
    readVar  :: r -> m a
    -- |Write new value into an 'Mutable'
    writeVar :: r -> a -> m ()

-- |Modify the contents of an 'Mutable' by applying pure function to it
{-# INLINE modifyVar #-}
modifyVar  var f  =  readVar var >>= writeVar var . f

-- |Modify the contents of an 'Mutable' by applying monadic computation to it
{-# INLINE modifyVarM #-}
modifyVarM var f  =  readVar var >>= f >>= writeVar var

-- -----------------------------------------------------------------------------
-- Implementation of `Mutable` interface for references

instance Mutable IO (IORef a) a where
    readVar  = readRef
    writeVar = writeRef
instance Mutable (ST s) (STRef s a) a where
    readVar  = readRef
    writeVar = writeRef
instance (Unboxed a) => Mutable IO (IOURef a) a where
    readVar  = readURef
    writeVar = writeURef
    {-# INLINE readVar  #-}
    {-# INLINE writeVar #-}
instance (Unboxed a) => Mutable (ST s) (STURef s a) a where
    readVar  = readURef
    writeVar = writeURef

-- -----------------------------------------------------------------------------
-- Implementation of `Mutable` interface for elements of MArray,
--   including simplified interfaces for 2-dimensional and 3-dimensional arrays

instance (Ix i) => Mutable IO (IOArray i e, i) e where
    readVar  (arr,i) = readArray  arr i
    writeVar (arr,i) = writeArray arr i

instance (Unboxed e, Ix i) => Mutable IO (IOUArray i e, i) e where
    readVar  (arr,i) = readArray  arr i
    writeVar (arr,i) = writeArray arr i

instance (Storable e, Ix i) => Mutable IO (StorableArray i e, i) e where
    readVar  (arr,i) = readArray  arr i
    writeVar (arr,i) = writeArray arr i

instance (Ix i) => Mutable (ST s) (STArray s i e, i) e where
    readVar  (arr,i) = readArray  arr i
    writeVar (arr,i) = writeArray arr i

instance (Unboxed e, Ix i) => Mutable (ST s) (STUArray s i e, i) e where
    readVar  (arr,i) = readArray  arr i
    writeVar (arr,i) = writeArray arr i

instance (MArray a e m, Ix i, Ix j) => Mutable m (a (i,j) e, i, j) e where
    readVar  (arr,i,j) = readArray  arr (i,j)
    writeVar (arr,i,j) = writeArray arr (i,j)

instance (MArray a e m, Ix i, Ix j, Ix k) => Mutable m (a (i,j,k) e, i, j, k) e where
    readVar  (arr,i,j,k) = readArray  arr (i,j,k)
    writeVar (arr,i,j,k) = writeArray arr (i,j,k)

-- -----------------------------------------------------------------------------
-- Implementation of `Mutable` interface for values in HashTable

instance Mutable IO (HashTable key e, key) e where
    readVar  (table,key)   = do (Just x) <- Hash.lookup table key
                                return x
    writeVar (table,key) e = hashUpdate table key e >> return ()

#if defined(__HUGS_VERSION__)  ||  defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ >= 604)
-- Define this only for Hugs2005+ and GHC 6.4+
hashUpdate table = Hash.update table
#else
-- Slower implementation for old compilers
hashUpdate table key e = do Hash.delete table key
                            Hash.insert table key e
#endif


-- -----------------------------------------------------------------------------
-- Syntax sugar for using mutables

infixl 0 =:, +=, -=, .=, .<-
ref  x  = newRef  x                           -- create new boxed reference
uref x  = newURef x                           -- create new unboxed reference
val var = readVar    var                      -- read current value of mutable
var=:x  = writeVar   var x                    -- assign new value to mutable
var+=x  = modifyVar  var (\old -> old+x)      -- increase value of mutable
var-=x  = modifyVar  var (\old -> old-x)      -- decrease value of mutable
var.=f  = modifyVar  var (\old -> f old)      -- apply pure function to the value of mutable
var.<-f = modifyVarM var (\old -> f old)      -- apply monadic computation to the value of mutable

{-# INLINE ref  #-}
{-# INLINE uref #-}
{-# INLINE (=:) #-}
{-# INLINE (+=) #-}
{-# INLINE (-=) #-}

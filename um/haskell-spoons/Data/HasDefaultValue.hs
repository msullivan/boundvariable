{-# OPTIONS_GHC -cpp -fglasgow-exts #-}
{- |
   Module     : Data.HasDefaultValue
   Copyright  : Copyright (C) 2006 Bulat Ziganshin
   License    : BSD3

   Maintainer : Bulat Ziganshin <Bulat.Ziganshin@gmail.com>
   Stability  : experimental
   Portability: portable

Class 'HasDefaultValue' allows to declare type's default value

-}

module Data.HasDefaultValue (
           HasDefaultValue(..),
       )
where

import Data.Int
import Data.Word
import Foreign.Ptr
import Foreign.StablePtr

-- ---------------------------------------------------------------------------
-- | Types that has default value

class HasDefaultValue a where
    defaultValue :: a

instance HasDefaultValue Bool          where defaultValue = False
instance HasDefaultValue Char          where defaultValue = '\0'
instance HasDefaultValue Int           where defaultValue = 0
instance HasDefaultValue Int8          where defaultValue = 0
instance HasDefaultValue Int16         where defaultValue = 0
instance HasDefaultValue Int32         where defaultValue = 0
instance HasDefaultValue Int64         where defaultValue = 0
#if !defined(__HUGS__) || defined(__HUGS_VERSION__)
-- don't define this for Hugs2003
instance HasDefaultValue Word          where defaultValue = 0
#endif
instance HasDefaultValue Word8         where defaultValue = 0
instance HasDefaultValue Word16        where defaultValue = 0
instance HasDefaultValue Word32        where defaultValue = 0
instance HasDefaultValue Word64        where defaultValue = 0
instance HasDefaultValue Float         where defaultValue = 0
instance HasDefaultValue Double        where defaultValue = 0
instance HasDefaultValue (Ptr a)       where defaultValue = nullPtr
instance HasDefaultValue (FunPtr a)    where defaultValue = nullFunPtr
instance HasDefaultValue (StablePtr a) where defaultValue = castPtrToStablePtr nullPtr


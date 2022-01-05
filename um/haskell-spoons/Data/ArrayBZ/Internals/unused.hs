
This file contains remaining code from old Data.Array.* library that is still
unused in new code. It mainly consists of speed tricks (that may be even not
needed in new code).

------------------------------------------------------------------------------------------
class HasBounds a => IArray a e where
    unsafeReplace arr ies = runST (unsafeReplaceST arr ies >>= unsafeFreeze)
    unsafeAccum f arr ies = runST (unsafeAccumST f arr ies >>= unsafeFreeze)
    unsafeAccumArray f e lu ies = runST (unsafeAccumArrayST f e lu ies >>= unsafeFreeze)

{-# INLINE unsafeReplaceST #-}
unsafeReplaceST :: (IArray a e, Ix i) => a i e -> [(Int, e)] -> ST s (STArray s i e)
unsafeReplaceST arr ies = do
    marr <- thaw arr
    sequence_ [unsafeWrite marr i e | (i, e) <- ies]
    return marr

{-# INLINE unsafeAccumST #-}
unsafeAccumST :: (IArray a e, Ix i) => (e -> e' -> e) -> a i e -> [(Int, e')] -> ST s (STArray s i e)
unsafeAccumST f arr ies = do
    marr <- thaw arr
    sequence_ [do
        old <- unsafeRead marr i
        unsafeWrite marr i (f old new)
        | (i, new) <- ies]
    return marr

{-# INLINE unsafeAccumArrayST #-}
unsafeAccumArrayST :: Ix i => (e -> e' -> e) -> e -> (i,i) -> [(Int, e')] -> ST s (STArray s i e)
unsafeAccumArrayST f e (l,u) ies = do
    marr <- newArray (l,u) e
    sequence_ [do
        old <- unsafeRead marr i
        unsafeWrite marr i (f old new)
        | (i, new) <- ies]
    return marr

------------------------------------------------------------------------------------------
{-# INLINE listArrayST #-}
listArrayST :: Ix i => (i,i) -> [e] -> ST s (STArray s i e)
listArrayST (l,u) es = do
    marr <- newArray_ (l,u)
    let n = rangeSize (l,u)
    let fillFromList i xs | i == n    = return ()
                          | otherwise = case xs of
            []   -> return ()
            y:ys -> unsafeWrite marr i y >> fillFromList (i+1) ys
    fillFromList 0 es
    return marr

{-# RULES
"listArray/Array" listArray =
    \lu es -> runST (listArrayST lu es >>= ArrST.unsafeFreezeSTArray)
    #-}

{-# INLINE listUArrayST #-}
listUArrayST :: (MArray (STUArray s) e (ST s), Ix i)
             => (i,i) -> [e] -> ST s (STUArray s i e)
listUArrayST (l,u) es = do
    marr <- newArray_ (l,u)
    let n = rangeSize (l,u)
    let fillFromList i xs | i == n    = return ()
                          | otherwise = case xs of
            []   -> return ()
            y:ys -> unsafeWrite marr i y >> fillFromList (i+1) ys
    fillFromList 0 es
    return marr

-- I don't know how to write a single rule for listUArrayST, because
-- the type looks like constrained over 's', which runST doesn't
-- like. In fact all MArray (STUArray s) instances are polymorphic
-- wrt. 's', but runST can't know that.
--
-- More precisely, we'd like to write this:
--   listUArray :: (forall s. MArray (STUArray s) e (ST s), Ix i)
--              => (i,i) -> [e] -> UArray i e
--   listUArray lu = runST (listUArrayST lu es >>= unsafeFreezeSTUArray)
--   {-# RULES listArray = listUArray
-- Then we could call listUArray at any type 'e' that had a suitable
-- MArray instance.  But sadly we can't, because we don't have quantified
-- constraints.  Hence the mass of rules below.

-- I would like also to write a rule for listUArrayST (or listArray or
-- whatever) applied to unpackCString#. Unfortunately unpackCString#
-- calls seem to be floated out, then floated back into the middle
-- of listUArrayST, so I was not able to do this.

#ifdef __GLASGOW_HASKELL__
type ListUArray e = forall i . Ix i => (i,i) -> [e] -> UArray i e

{-# RULES
"listArray/UArray/Bool"      listArray
   = (\lu es -> runST (listUArrayST lu es >>= unsafeFreezeSTUArray)) :: ListUArray Bool
"listArray/UArray/Char"      listArray
   = (\lu es -> runST (listUArrayST lu es >>= unsafeFreezeSTUArray)) :: ListUArray Char
"listArray/UArray/Int"       listArray
   = (\lu es -> runST (listUArrayST lu es >>= unsafeFreezeSTUArray)) :: ListUArray Int
"listArray/UArray/Word"      listArray
   = (\lu es -> runST (listUArrayST lu es >>= unsafeFreezeSTUArray)) :: ListUArray Word
"listArray/UArray/Ptr"       listArray
   = (\lu es -> runST (listUArrayST lu es >>= unsafeFreezeSTUArray)) :: ListUArray (Ptr a)
"listArray/UArray/FunPtr"    listArray
   = (\lu es -> runST (listUArrayST lu es >>= unsafeFreezeSTUArray)) :: ListUArray (FunPtr a)
"listArray/UArray/Float"     listArray
   = (\lu es -> runST (listUArrayST lu es >>= unsafeFreezeSTUArray)) :: ListUArray Float
"listArray/UArray/Double"    listArray
   = (\lu es -> runST (listUArrayST lu es >>= unsafeFreezeSTUArray)) :: ListUArray Double
"listArray/UArray/StablePtr" listArray
   = (\lu es -> runST (listUArrayST lu es >>= unsafeFreezeSTUArray)) :: ListUArray (StablePtr a)
"listArray/UArray/Int8"      listArray
   = (\lu es -> runST (listUArrayST lu es >>= unsafeFreezeSTUArray)) :: ListUArray Int8
"listArray/UArray/Int16"     listArray
   = (\lu es -> runST (listUArrayST lu es >>= unsafeFreezeSTUArray)) :: ListUArray Int16
"listArray/UArray/Int32"     listArray
   = (\lu es -> runST (listUArrayST lu es >>= unsafeFreezeSTUArray)) :: ListUArray Int32
"listArray/UArray/Int64"     listArray
   = (\lu es -> runST (listUArrayST lu es >>= unsafeFreezeSTUArray)) :: ListUArray Int64
"listArray/UArray/Word8"     listArray
   = (\lu es -> runST (listUArrayST lu es >>= unsafeFreezeSTUArray)) :: ListUArray Word8
"listArray/UArray/Word16"    listArray
   = (\lu es -> runST (listUArrayST lu es >>= unsafeFreezeSTUArray)) :: ListUArray Word16
"listArray/UArray/Word32"    listArray
   = (\lu es -> runST (listUArrayST lu es >>= unsafeFreezeSTUArray)) :: ListUArray Word32
"listArray/UArray/Word64"    listArray
   = (\lu es -> runST (listUArrayST lu es >>= unsafeFreezeSTUArray)) :: ListUArray Word64
    #-}
#endif

------------------------------------------------------------------------------------------
instance MArray (STArray s) e (Lazy.ST s) where
    {-# INLINE newArray #-}
    newArray (l,u) e    = strictToLazyST (ArrST.newSTArray (l,u) e)
    {-# INLINE unsafeRead #-}
    unsafeRead arr i    = strictToLazyST (ArrST.unsafeReadSTArray arr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite arr i e = strictToLazyST (ArrST.unsafeWriteSTArray arr i e)

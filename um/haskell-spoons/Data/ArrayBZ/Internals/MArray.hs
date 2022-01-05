{-# OPTIONS_GHC -cpp -fglasgow-exts #-}
{- |
   Module     : Data.ArrayBZ.Internals.MArray
   Copyright  : (c) The University of Glasgow 2001 & (c) 2006 Bulat Ziganshin
   License    : BSD3

   Maintainer : Bulat Ziganshin <Bulat.Ziganshin@gmail.com>
   Stability  : experimental
   Portability: GHC/Hugs

Mutable arrays: class and general algorithms
Freeze/Thaw: mutable<->immutable arrays conversion

-}

module Data.ArrayBZ.Internals.MArray where

import Control.Monad.ST         ( ST, runST )
import Data.Ix
#ifdef __GLASGOW_HASKELL__
import GHC.Arr                  ( unsafeIndex )
#endif
#ifdef __HUGS__
import Hugs.Array               ( unsafeIndex )
#endif

import Data.ArrayBZ.Internals.IArray

-----------------------------------------------------------------------------
-- Mutable arrays

-- | Value used to initialize undefined array elements
{-# NOINLINE arrEleBottom #-}
arrEleBottom :: a
arrEleBottom = error "MArray: undefined array element"

-- | Class of array types with mutable bounds
class (Monad m) => HasMutableBounds a m where
    -- | Get the current bounds of an array
    getBounds :: Ix i => a i e -> m (i,i)

{-| Class of mutable array types.

An array type has the form @(a i e)@ where @a@ is the array type
constructor (kind @* -> * -> *@), @i@ is the index type (a member of
the class 'Ix'), and @e@ is the element type.

The @MArray@ class is parameterised over both @a@ and @e@ (so that
instances specialised to certain element types can be defined, in the
same way as for 'IArray'), and also over the type of the monad, @m@,
in which the mutable array will be manipulated.
-}
class (Monad m, HasMutableBounds a m) => MArray a e m where

    -- | Builds a new array, with every element initialised to the supplied
    -- value.
    newArray    :: Ix i => (i,i) -> e -> m (a i e)

    -- | Builds a new array, with every element initialised to undefined.
    newArray_   :: Ix i => (i,i) -> m (a i e)

    unsafeRead  :: Ix i => a i e -> Int -> m e
    unsafeWrite :: Ix i => a i e -> Int -> e -> m ()

    readArray   :: Ix i => a i e -> i -> m e
    writeArray  :: Ix i => a i e -> i -> e -> m ()

    {-# INLINE newArray #-}
        -- The INLINE is crucial, because until we know at least which monad
        -- we are in, the code below allocates like crazy.  So inline it,
        -- in the hope that the context will know the monad.
    newArray (l,u) init = do
        marr <- newArray_ (l,u)
        sequence_ [unsafeWrite marr i init | i <- [0 .. rangeSize (l,u) - 1]]
        return marr

    newArray_ (l,u) = newArray (l,u) arrEleBottom

    -- newArray takes an initialiser which all elements of
    -- the newly created array are initialised to.  newArray_ takes
    -- no initialiser, it is assumed that the array is initialised with
    -- "undefined" values.

    -- why not omit newArray_?  Because in the unboxed array case we would
    -- like to omit the initialisation altogether if possible.  We can't do
    -- this for boxed arrays, because the elements must all have valid values
    -- at all times in case of garbage collection.

    -- why not omit newArray?  Because in the boxed case, we can omit the
    -- default initialisation with undefined values if we *do* know the
    -- initial value and it is constant for all elements.

    {-# INLINE readArray #-}
    -- | Read an element from a mutable array
    readArray marr i = do
        (l,u) <- getBounds marr
        unsafeRead marr (index (l,u) i)

    {-# INLINE writeArray #-}
    -- | Write an element in a mutable array
    writeArray marr i e = do
        (l,u) <- getBounds marr
        unsafeWrite marr (index (l,u) i) e


-----------------------------------------------------------------------------
-- Algorithms on mutable arrays

{-# INLINE newListArray #-}
-- | Constructs a mutable array from a list of initial elements.
-- The list gives the elements of the array in ascending order
-- beginning with the lowest index.
newListArray :: (MArray a e m, Ix i) => (i,i) -> [e] -> m (a i e)
newListArray (l,u) es = do
    marr <- newArray_ (l,u)
    let n = rangeSize (l,u)
    let fillFromList i xs | i == n    = return ()
                          | otherwise = case xs of
            []   -> return ()
            y:ys -> unsafeWrite marr i y >> fillFromList (i+1) ys
    fillFromList 0 es
    return marr

{-# INLINE getIndices #-}
-- | Return a list of all the indexes of a mutable array
getIndices :: (MArray a e m, Ix i) => a i e -> m [i]
getIndices marr = do
    (l,u) <- getBounds marr
    return (range (l,u))

{-# INLINE getElems #-}
-- | Return a list of all the elements of a mutable array
getElems :: (MArray a e m, Ix i) => a i e -> m [e]
getElems marr = do
    (l,u) <- getBounds marr
    sequence [unsafeRead marr i | i <- [0 .. rangeSize (l,u) - 1]]

{-# INLINE getAssocs #-}
-- | Return a list of all the associations of a mutable array, in
-- index order.
getAssocs :: (MArray a e m, Ix i) => a i e -> m [(i, e)]
getAssocs marr = do
    (l,u) <- getBounds marr
    sequence [ do e <- unsafeRead marr (index (l,u) i); return (i,e)
             | i <- range (l,u)]

{-# INLINE mapArray #-}
-- | Constructs a new array derived from the original array by applying a
-- function to each of the elements.
mapArray :: (MArray a e' m, MArray a e m, Ix i) => (e' -> e) -> a i e' -> m (a i e)
mapArray f marr = do
    (l,u) <- getBounds marr
    marr' <- newArray_ (l,u)
    sequence_ [do
        e <- unsafeRead marr i
        unsafeWrite marr' i (f e)
        | i <- [0 .. rangeSize (l,u) - 1]]
    return marr'

{-# INLINE mapIndices #-}
-- | Constructs a new array derived from the original array by applying a
-- function to each of the indices.
mapIndices :: (MArray a e m, Ix i, Ix j) => (i,i) -> (i -> j) -> a j e -> m (a i e)
mapIndices (l,u) f marr = do
    marr' <- newArray_ (l,u)
    sequence_ [do
        e <- readArray marr (f i)
        unsafeWrite marr' (unsafeIndex (l,u) i) e
        | i <- range (l,u)]
    return marr'

-----------------------------------------------------------------------------
-- Freezing

-- | Converts a mutable array (any instance of 'MArray') to an
-- immutable array (any instance of 'IArray') by taking a complete
-- copy of it.
freeze :: (Ix i, MArray a e m, IArray b e) => a i e -> m (b i e)
freeze marr = do
    (l,u) <- getBounds marr
    ies <- sequence [do e <- unsafeRead marr i; return (i,e)
                     | i <- [0 .. rangeSize (l,u) - 1]]
    return (unsafeArray (l,u) ies)

-- In-place conversion of mutable arrays to immutable ones places
-- a proof obligation on the user: no other parts of your code can
-- have a reference to the array at the point where you unsafely
-- freeze it (and, subsequently mutate it, I suspect).

{- |
   Converts an mutable array into an immutable array.  The
   implementation may either simply cast the array from
   one type to the other without copying the array, or it
   may take a full copy of the array.

   Note that because the array is possibly not copied, any subsequent
   modifications made to the mutable version of the array may be
   shared with the immutable version.  It is safe to use, therefore, if
   the mutable version is never modified after the freeze operation.

   The non-copying implementation is supported between certain pairs
   of array types only; one constraint is that the array types must
   have identical representations.  In GHC, The following pairs of
   array types have a non-copying O(1) implementation of
   'unsafeFreeze'.  Because the optimised versions are enabled by
   specialisations, you will need to compile with optimisation (-O) to
   get them.

     * 'Data.Array.IO.IOUArray' -> 'Data.Array.Unboxed.UArray'

     * 'Data.Array.ST.STUArray' -> 'Data.Array.Unboxed.UArray'

     * 'Data.Array.IO.IOArray' -> 'Data.Array.Array'

     * 'Data.Array.ST.STArray' -> 'Data.Array.Array'
-}
{-# INLINE unsafeFreeze #-}
unsafeFreeze :: (Ix i, MArray a e m, IArray b e) => a i e -> m (b i e)
unsafeFreeze = freeze

-----------------------------------------------------------------------------
-- Thawing

-- | Converts an immutable array (any instance of 'IArray') into a
-- mutable array (any instance of 'MArray') by taking a complete copy
-- of it.
thaw :: (Ix i, IArray a e, MArray b e m) => a i e -> m (b i e)
thaw arr = case bounds arr of
  (l,u) -> do
    marr <- newArray_ (l,u)
    sequence_ [unsafeWrite marr i (unsafeAt arr i)
               | i <- [0 .. rangeSize (l,u) - 1]]
    return marr

-- In-place conversion of immutable arrays to mutable ones places
-- a proof obligation on the user: no other parts of your code can
-- have a reference to the array at the point where you unsafely
-- thaw it (and, subsequently mutate it, I suspect).

{- |
   Converts an immutable array into a mutable array.  The
   implementation may either simply cast the array from
   one type to the other without copying the array, or it
   may take a full copy of the array.

   Note that because the array is possibly not copied, any subsequent
   modifications made to the mutable version of the array may be
   shared with the immutable version.  It is only safe to use,
   therefore, if the immutable array is never referenced again in this
   thread, and there is no possibility that it can be also referenced
   in another thread.  If you use an unsafeThaw/write/unsafeFreeze
   sequence in a multi-threaded setting, then you must ensure that
   this sequence is atomic with respect to other threads, or a garbage
   collector crash may result (because the write may be writing to a
   frozen array).

   The non-copying implementation is supported between certain pairs
   of array types only; one constraint is that the array types must
   have identical representations.  In GHC, The following pairs of
   array types have a non-copying O(1) implementation of
   'unsafeThaw'.  Because the optimised versions are enabled by
   specialisations, you will need to compile with optimisation (-O) to
   get them.

     * 'Data.Array.Unboxed.UArray' -> 'Data.Array.IO.IOUArray'

     * 'Data.Array.Unboxed.UArray' -> 'Data.Array.ST.STUArray'

     * 'Data.Array.Array'  -> 'Data.Array.IO.IOArray'

     * 'Data.Array.Array'  -> 'Data.Array.ST.STArray'
-}
{-# INLINE unsafeThaw #-}
unsafeThaw :: (Ix i, IArray a e, MArray b e m) => a i e -> m (b i e)
unsafeThaw = thaw


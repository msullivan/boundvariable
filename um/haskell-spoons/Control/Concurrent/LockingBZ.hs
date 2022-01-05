{-# OPTIONS_GHC -cpp -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.LockingBZ
-- Copyright   :  (c) Bulat Ziganshin <Bulat.Ziganshin@gmail.com> 2006
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (concurrency)
--
-- Attaching lock to immutable value.
--
-----------------------------------------------------------------------------

module Control.Concurrent.LockingBZ
  (
          -- * Locking

          -- $intro

          -- ** The 'WithLocking h' type constructor
        WithLocking(..),        -- instance of Show

          -- ** Attaching lock to value
        addLocking,             -- :: h -> IO (WithLocking h)
        withLocking,            -- :: h -> (WithLocking h -> IO a) -> IO a

          -- ** Using value inside lock
#if defined (__GLASGOW_HASKELL__) || defined (__HUGS__)
        Locking(..),
#else
        lock,
#endif

          -- ** Promoting operations to use locks
        liftLock1,
        liftLock2,
        liftLock3,
        liftLock4,
        liftLock5,

   ) where

import Control.Concurrent.MVar
import Control.Exception as Exception

{- $intro

This library allows to attach lock to any immutable value so that access
to this value can be obtained only via the 'lock' operation that ensures
that this value will never be used at the same time by concurrent threads.
Lock attached to value by 'addLocking' operation, it's also possible to run
code block with locked version of some value by 'withLocking' operation.

To work with value contained inside lock, you should use 'lock' operation;
it's usage is very like to using 'withMVar' for the same purpose, but you
don't got ability to return new value of internal data from the action
performed. On the other side, 'lock' operation is about two times faster
than 'withMVar' according to my tests. There are also 'liftLock*'
operations that simplifies promoting operations on original value to
operations on it's locked version. Hugs/GHC version of this library defines
'lock' as operation of class 'Locking' that opens possibility to define
alternative 'lock' implementations.

First usage example - adding lock to mutable array and promoting the mutable
array with lock to support mutable array interface again. This can be done
with any objects what are accessed through some interface defined via type
class:

>   import Control.Concurrent.Locking
>
>   type WithLocking2 a e m = WithLocking (a e m)
>
>   instance (MArray a e m) => (MArray (WithLocking2 a) e m) where
>       newArray lu e = newArray lu e >>= addLocking
>       newArray_ lu  = newArray_ lu  >>= addLocking
>       unsafeRead = liftLock2 unsafeRead
>       unsafeWrite = liftLock3 unsafeWrite
>
>   main = do arr <- newArray (0,9) 0 >>= addLocking
>             readArray arr 0 >>= writeArray arr 1
>             .....
>

Another example where 'lock' operation used to get exclusive access to file
while performing sequence of operations on it:

>   main = do lh <- openBinaryFile "test" ReadMode >>= addLocking
>             ....
>             str <- readStringAt lh pos
>             ....
>
>   readStringAt lh pos =
>       lock lh $ \h -> do
>           saved_pos <- hTell h
>           hSeek h AbsoluteSeek pos
>           str <- hGetLine h
>           hSeek h AbsoluteSeek saved_pos
>           return str

In this example, any thread can use 'readStringAt' on the same locked handle
without risk to interfere with each other's operation

-}


-- -----------------------------------------------------------------------------
-- 'WithLocking' type constructor and it's constructor functions

-- | Type constructor that attaches lock to immutable value @h@
data WithLocking h = WithLocking h !(MVar ())

instance (Show h) => Show (WithLocking h) where
    show (WithLocking h _) = "WithLocking ("++ show h ++")"

-- | Add lock to object to ensure it's proper use in concurrent threads
addLocking :: h -> IO (WithLocking h)
addLocking h = do
    mvar <- newMVar ()
    return (WithLocking h mvar)

-- | Run @action@ with locked version of object
withLocking :: h -> (WithLocking h -> IO a) -> IO a
withLocking h action = do
    addLocking h >>= action

-- -----------------------------------------------------------------------------
-- 'lock' operation definition: use MPTC+FD for Hugs/GHC or simple function for
-- compilers that don't support MPTC+FD

#if defined (__GLASGOW_HASKELL__) || defined (__HUGS__)

-- | Define class of locking implementations, where 'lh' holds lock around 'h'
class Locking lh h | lh->h where
    -- | Perform action while exclusively locking wrapped object
    -- (faster analog of using 'withMVar' for the same purpose)
    lock :: lh -> (h->IO a) -> IO a

instance Locking (WithLocking h) h where
    {-# INLINE lock #-}
    lock

#else

{-# INLINE lock #-}
-- | Perform action while exclusively locking wrapped object
-- (faster analog of using 'withMVar' for the same purpose)
lock :: (WithLocking h) -> (h->IO a) -> IO a
lock

#endif
     (WithLocking h mvar) action = do
        Exception.catch (do takeMVar mvar
                            result <- action h
                            putMVar mvar ()
                            return result
                        )
                        (\e -> do tryPutMVar mvar (); throw e)

-- -----------------------------------------------------------------------------
-- Helper operations - wrappers around 'lock'

{-# INLINE liftLock1 #-}
-- | Lift 1-parameter action to operation on locked variable
liftLock1 action h         = lock h (\a -> action a)

{-# INLINE liftLock2 #-}
-- | Lift 2-parameter action to operation on locked variable
liftLock2 action h x       = lock h (\a -> action a x)

{-# INLINE liftLock3 #-}
-- | Lift 3-parameter action to operation on locked variable
liftLock3 action h x y     = lock h (\a -> action a x y)

{-# INLINE liftLock4 #-}
-- | Lift 4-parameter action to operation on locked variable
liftLock4 action h x y z   = lock h (\a -> action a x y z)

{-# INLINE liftLock5 #-}
-- | Lift 5-parameter action to operation on locked variable
liftLock5 action h x y z t = lock h (\a -> action a x y z t)


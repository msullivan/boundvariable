{-# OPTIONS_GHC -cpp -fglasgow-exts -#include "HsBase.h" #-}
{- |
   Module     : Data.ArrayBZ.IO
   Copyright  : (c) The University of Glasgow 2001 & (c) 2006 Bulat Ziganshin
   License    : BSD3

   Maintainer : Bulat Ziganshin <Bulat.Ziganshin@gmail.com>
   Stability  : experimental
   Portability: Hugs/GHC

Mutable boxed and unboxed arrays in the IO monad.

-}

module Data.ArrayBZ.IO (
   -- * @IO@ arrays with boxed elements
   IOArray,             -- instance of: Eq, Typeable

   -- * @IO@ arrays with unboxed elements
   IOUArray,            -- instance of: Eq, Typeable
   castIOUArray,        -- :: IOUArray i a -> IO (IOUArray i b)

   -- * Overloaded mutable array interface
   module Data.ArrayBZ.MArray,

   -- * Doing I\/O with @IOUArray@s
   hGetArray,           -- :: Handle -> IOUArray Int Word8 -> Int -> IO Int
   hPutArray,           -- :: Handle -> IOUArray Int Word8 -> Int -> IO ()
 ) where

import Data.Word

import Data.ArrayBZ.MArray
import Data.ArrayBZ.Internals.Boxed   ( IOArray )
import Data.ArrayBZ.Internals.Unboxed

#ifdef __GLASGOW_HASKELL__
import Foreign
import Foreign.C

import GHC.IOBase hiding (IOArray)
import GHC.Handle
import GHC.Unboxed

-- ---------------------------------------------------------------------------
-- hGetArray

-- | Reads a number of 'Word8's from the specified 'Handle' directly
-- into an array.
hGetArray
        :: Handle               -- ^ Handle to read from
        -> IOUArray Int Word8   -- ^ Array in which to place the values
        -> Int                  -- ^ Number of 'Word8's to read
        -> IO Int
                -- ^ Returns: the number of 'Word8's actually
                -- read, which might be smaller than the number requested
                -- if the end of file was reached.

hGetArray handle (UMA l u (MUVec ptr)) count
  | count == 0
  = return 0
  | count < 0 || count > rangeSize (l,u)
  = illegalBufferSize handle "hGetArray" count
  | otherwise = do
      wantReadableHandle "hGetArray" handle $
        \ handle_@Handle__{ haFD=fd, haBuffer=ref, haIsStream=is_stream } -> do
        buf@Buffer{ bufBuf=raw, bufWPtr=w, bufRPtr=r } <- readIORef ref
        if bufferEmpty buf
           then readChunk fd is_stream ptr 0 count
           else do
                let avail = w - r
                copied <- if (count >= avail)
                            then do
                                memcpy_ba_baoff ptr raw r (fromIntegral avail)
                                writeIORef ref buf{ bufWPtr=0, bufRPtr=0 }
                                return avail
                            else do
                                memcpy_ba_baoff ptr raw r (fromIntegral count)
                                writeIORef ref buf{ bufRPtr = r + count }
                                return count

                let remaining = count - copied
                if remaining > 0
                   then do rest <- readChunk fd is_stream ptr copied remaining
                           return (rest + copied)
                   else return count

readChunk :: FD -> Bool -> RawBuffer -> Int -> Int -> IO Int
readChunk fd is_stream ptr init_off bytes = loop init_off bytes
 where
  loop :: Int -> Int -> IO Int
  loop off bytes | bytes <= 0 = return (off - init_off)
  loop off bytes = do
    r' <- readRawBuffer "readChunk" (fromIntegral fd) is_stream ptr
                                    (fromIntegral off) (fromIntegral bytes)
    let r = fromIntegral r'
    if r == 0
        then return (off - init_off)
        else loop (off + r) (bytes - r)

-- ---------------------------------------------------------------------------
-- hPutArray

-- | Writes an array of 'Word8' to the specified 'Handle'.
hPutArray
        :: Handle                       -- ^ Handle to write to
        -> IOUArray Int Word8           -- ^ Array to write from
        -> Int                          -- ^ Number of 'Word8's to write
        -> IO ()

hPutArray handle (UMA l u (MUVec raw)) count
  | count == 0
  = return ()
  | count < 0 || count > rangeSize (l,u)
  = illegalBufferSize handle "hPutArray" count
  | otherwise
   = do wantWritableHandle "hPutArray" handle $
          \ handle_@Handle__{ haFD=fd, haBuffer=ref, haIsStream=stream } -> do

          old_buf@Buffer{ bufBuf=old_raw, bufRPtr=r, bufWPtr=w, bufSize=size }
            <- readIORef ref

          -- enough room in handle buffer?
          if (size - w > count)
                -- There's enough room in the buffer:
                -- just copy the data in and update bufWPtr.
            then do memcpy_baoff_ba old_raw w raw (fromIntegral count)
                    writeIORef ref old_buf{ bufWPtr = w + count }
                    return ()

                -- else, we have to flush
            else do flushed_buf <- flushWriteBuffer fd stream old_buf
                    writeIORef ref flushed_buf
                    let this_buf =
                            Buffer{ bufBuf=raw, bufState=WriteBuffer,
                                    bufRPtr=0, bufWPtr=count, bufSize=count }
                    flushWriteBuffer fd stream this_buf
                    return ()

-- ---------------------------------------------------------------------------
-- Internal Utils

foreign import ccall unsafe "__hscore_memcpy_dst_off"
   memcpy_baoff_ba :: RawBuffer -> Int -> RawBuffer -> CSize -> IO (Ptr ())
foreign import ccall unsafe "__hscore_memcpy_src_off"
   memcpy_ba_baoff :: RawBuffer -> RawBuffer -> Int -> CSize -> IO (Ptr ())

illegalBufferSize :: Handle -> String -> Int -> IO a
illegalBufferSize handle fn sz =
        ioException (IOError (Just handle)
                            InvalidArgument  fn
                            ("illegal buffer size " ++ showsPrec 9 (sz::Int) [])
                            Nothing)

#else /* !__GLASGOW_HASKELL__ */
import Data.Char
import System.IO
import System.IO.Error
import Data.ArrayBZ.Internals.MArray

hGetArray :: Handle -> IOUArray Int Word8 -> Int -> IO Int
hGetArray handle arr count
  | count < 0 || count > rangeSize (bounds arr)
  = illegalBufferSize handle "hGetArray" count
  | otherwise = get 0
 where
  get i | i == count = return i
        | otherwise = do
                error_or_c <- try (hGetChar handle)
                case error_or_c of
                    Left ex
                        | isEOFError ex -> return i
                        | otherwise -> ioError ex
                    Right c -> do
                        unsafeWrite arr i (fromIntegral (ord c))
                        get (i+1)

hPutArray :: Handle -> IOUArray Int Word8 -> Int -> IO ()
hPutArray handle arr count
  | count < 0 || count > rangeSize (bounds arr)
  = illegalBufferSize handle "hPutArray" count
  | otherwise = put 0
 where
  put i | i == count = return ()
        | otherwise = do
                w <- unsafeRead arr i
                hPutChar handle (chr (fromIntegral w))
                put (i+1)

illegalBufferSize :: Handle -> String -> Int -> IO a
illegalBufferSize _ fn sz = ioError $
        userError (fn ++ ": illegal buffer size " ++ showsPrec 9 (sz::Int) [])
#endif /* !__GLASGOW_HASKELL__ */

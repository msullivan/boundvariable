module Main
    where

import System
import System.IO

import Data.Char
import Data.Word
import Data.Array.IO
import qualified Data.ArrayBZ.Dynamic as D
import Bits

type Platter = IOUArray Word32 Word32
type Registers = Platter

-- Print dynamic array bounds and contents
printDynamicArray arr = do
			bounds   <- D.getBounds arr
			contents <- D.getElems  arr
			putStrLn (show bounds++" : "++show contents)

printArray arr = do
		 contents <- getElems  arr
		 putStrLn (show contents)

--      code       ip        regs         memory                             high-mark freelist
step :: Platter -> Word32 -> Registers -> D.DynamicIOArray Word32 Platter -> Word32 -> [Word32] -> IO ()
step cd ip regs mem high free = do
--			       putStr ((show ip)++ " : ")
--			       printArray regs
			       inst <- readArray cd ip
			       case shiftR inst 28 of 
						   0 -> do t <- get (c inst)
							   if t /= 0 then do -- CMOV
									  src <- (get (b inst))
									  set (a inst) src
									  step cd (ip + 1) regs mem high free
							      else step cd (ip + 1) regs mem high free
						   1 -> do idx <- get (b inst) -- ASUB
							   off <- get (c inst)
							   arr <- D.readArray mem idx
							   v <- readArray arr off
							   set (a inst) v
							   step cd (ip + 1) regs mem high free
						   2 -> do idx <- get (a inst) -- UPD
							   off <- get (b inst)
							   arr <- D.readArray mem idx
							   v <- get (c inst)
							   writeArray arr off v
							   step cd (ip + 1) regs mem high free
						   3 -> do src1 <- get (b inst) -- ADD
							   src2 <- get (c inst)
							   set (a inst) (src1 + src2)
							   step cd (ip + 1) regs mem high free
						   4 -> do src1 <- get (b inst) -- MUL
							   src2 <- get (c inst)
							   set (a inst) (src1 * src2)
							   step cd (ip + 1) regs mem high free
						   5 -> do src1 <- get (b inst) -- DIV
							   src2 <- get (c inst)
							   set (a inst) (div src1 src2)
							   step cd (ip + 1) regs mem high free
						   6 -> do src1 <- get (b inst) -- NAND
							   src2 <- get (c inst)
							   set (a inst) (complement (src1 .&. src2))
							   step cd (ip + 1) regs mem high free
						   7 -> putStrLn "== HALTED. ==" -- HALT
						   8 -> do sz <- get (c inst) -- ALLOC
							   arr <- newArray (0, sz) 0
							   let (idx, free', high') = 
								   case free of
									     idx':free' -> (idx', free', high)
									     [] -> (high + 1, [], high + 1) in do
							   D.writeArray mem idx arr
							   set (b inst) idx
							   step cd (ip + 1) regs mem high' free'
						   9 -> do idx <- get (c inst)
						           -- PERF why allocate an empty?
							   empty <- newArray_ (fromIntegral 0, fromIntegral 0)
							   D.writeArray mem idx empty
							   step cd (ip + 1) regs mem high (idx:free)
						   10 -> do ch <- get (c inst) -- OUTPUT
							    putChar (chr (fromIntegral ch))
							    step cd (ip + 1) regs mem high free
						   11 -> do ch <- getChar -- INPUT
							    set (c inst) (fromIntegral (ord ch))
							    step cd (ip + 1) regs mem high free
						   12 -> do idx <- get (b inst)
							    off <- get (c inst)
							    if idx /= 0 then do arr <- D.readArray mem idx
										cd' <- mapArray id arr
										D.writeArray mem 0 cd'
										step cd' off regs mem high free
							       else step cd off regs mem high free
						   13 -> do set (aa inst) (lit inst)
							    step cd (ip + 1) regs mem high free
						   _ -> do
							putStrLn "unknown inst:"
							print inst
    where a inst = (shiftR inst 6) .&. 7
	  aa inst = (shiftR inst 25) .&. 7
	  b inst = (shiftR inst 3) .&. 7
	  c inst = inst .&. 7
	  lit inst = inst .&. 0x1FFFFFF
	  set r v = writeArray regs r v
	  get r = readArray regs r

run :: Platter -> IO ()
run cd = do 
	putStrLn "loaded."
	regs <- newArray (0, 7) 0
	mem <- D.newDynamicArray_ D.growTwoTimes (0, 3)
	D.writeArray mem 0 cd
	step cd 0 regs mem 0 []

readword :: Handle -> Platter -> Word32 -> IO Platter
readword h cd i = do 
		 eof <- hIsEOF h
		 if eof
		    then do hClose h
			    return cd
		    else do ch1 <- hGetChar h
			    ch2 <- hGetChar h
			    ch3 <- hGetChar h
			    ch4 <- hGetChar h
			    let w = (shift (ord ch1) 24) .|.
				    (shift (ord ch2) 16) .|.
				    (shift (ord ch3) 8) .|.
				    (ord ch4)
				in do
				   writeArray cd i (fromIntegral w)
				   readword h cd (i+1)

readfile :: FilePath -> IO Platter
readfile f = do
	     h <- openFile f ReadMode 
	     sz <- hFileSize h
	     cd <- newArray (fromIntegral 0, fromIntegral sz) 0 -- sz - 1?
	     readword h cd (fromIntegral 0)

go :: String -> IO ()
go f = do
       cd <- readfile f
       run cd

main :: IO ()
main = do
       cmd <- getArgs
       case cmd of
		[] -> putStrLn "usage XXX";
		[f] -> go f

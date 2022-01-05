import Data.Bits
import System (getArgs)
import Numeric (showHex)
import Char (toUpper)

data Inst = JNZ Int              -- IMM 
          | MATH (Int,Int,Int)   -- (dR,sR1,sR2)
          | LOGIC (Int,Int,Int)  -- (dR,sR1,sR2)
          | ROTREF Int           -- IMM
          | BAIL


readSr sr = 
    let
      i = read sr
    in
      if i > 3 || i < 0 then 
        (error ("Parse Error: " ++ sr ++ "  is not a valid source reg\n"))
      else
        i

readDr dr =
    let
      i = read dr
    in
      if i > 1 || i < 0 then
        (error ("Parse Error: " ++ dr ++ " is not a valid destination reg\n"))
      else
        i

readImm imm =
    let
      i = read imm
    in
      if i > 31 || i < -16 then
          (error ("Parse Error: " ++ imm ++ " is not a valid imm\n"))
      else
          if i < 0 then 32 + i else i

tokenize [] = []
tokenize (w:ws) =
    case w of
      "JNZ"    ->
          let
            (imm:ins) = ws
          in
            (JNZ (readImm imm)):(tokenize ins)            
      "MATH"   -> 
          let
            (dr:sr1:sr2:ins) = ws
          in
            (MATH (readDr dr, readSr sr1, readSr sr2)):(tokenize ins)
      "LOGIC"  -> 
          let
            (dr:sr1:sr2:ins) = ws
          in
            (LOGIC (readDr dr, readSr sr1, readSr sr2)):(tokenize ins)
      "ROTREF" ->
          let
            (imm:ins) = ws
          in
            (ROTREF (readImm imm)):(tokenize ins)
      "BAIL"   -> BAIL:(tokenize ws)
      _        -> error ("Parse Error: " ++ w ++ " is not a command\n")



make_int (JNZ imm) = imm
make_int (MATH (dr,sr1,sr2)) = (shiftL 1 5) .|. (shiftL dr 4) .|.
                               (shiftL sr1 2) .|. sr2
make_int (LOGIC (dr,sr1,sr2)) = (shiftL 2 5) .|. (shiftL dr 4) .|.
                                (shiftL sr1 2) .|. sr2
make_int (ROTREF imm) = (shiftL 3 5) .|. imm
make_int BAIL = (shiftL 7 5)


hexify i = if i > 15 then showHex i
           else \s -> ('0':(showHex i s))

main = do args <- getArgs
          if length args < 2 then
              print "Specify input and output files please\n"
            else
              let
                  (infile:outfile:_) = args
              in
                do input <- readFile infile
                   ints <- return (map make_int (tokenize 
                                                 (words (map toUpper input))))
                   writeFile outfile
                             (foldr (\num str -> (hexify num) str) "" ints)

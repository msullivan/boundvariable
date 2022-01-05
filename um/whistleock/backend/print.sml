
structure UMAPrint =
struct

  open UMA

  fun regtos 0 = "aa"
    | regtos 1 = "bb"
    | regtos 2 = "cc"
    | regtos 3 = "dd"
    | regtos 4 = "ee"
    | regtos 5 = "ff"
    | regtos 6 = "zz"
    | regtos 7 = "hh"
    | regtos _ = "XXX-bad-reg-XXX"

  fun tryprint (COMMENT _) = false
    | tryprint _           = true

  fun intos i =
    let
      val r = regtos
      val enc =
        if tryprint i 
        then
          (StringCvt.padLeft #"0" 8 (Word32.fmt StringCvt.HEX (UMA.encode i)))
          handle UMA.UMA _ => "XXXX?BAD"
        else                  "        "

      fun is i =
      (case i of
         CMOV (ra, rb, rc) => "cmov " ^ r ra ^ " <- " ^ r rb ^ " (?" ^ r rc ^ ")"
       | LOADPROG (ra, rb) => "loadprog " ^ r ra ^ " @ " ^ r rb
       | LITERAL (ra, w) => "literal " ^ r ra ^ " <- " ^ Word32.fmt StringCvt.HEX w
       | HALT => "halt"
       | WRITE ra => "write " ^ r ra
       | READ ra => "read " ^ r ra ^ " <- "
       | ALLOC (ra, rb) => "alloc " ^ r ra ^ " <- (" ^ r rb ^ " words)"
       | FREE ra => "free " ^ r ra
       | ASUB (ra, rb, rc) => "asub " ^ r ra ^ " <- " ^ r rb ^ "[" ^ r rc ^ "]"
       | UPD  (ra, rb, rc) => "upd " ^ r ra ^ "[" ^ r rb ^ "] <- " ^ r rc
       | ADD (ra, rb, rc) => "add " ^ r ra ^ " <- " ^ r rb ^ " + " ^ r rc
       | MUL (ra, rb, rc) => "mul " ^ r ra ^ " <- " ^ r rb ^ " * " ^ r rc
       | DIV (ra, rb, rc) => "div " ^ r ra ^ " <- " ^ r rb ^ " / " ^ r rc
       | NAND (ra, rb, rc) => "nand " ^ r ra ^ " <- " ^ r rb ^ " ~& " ^ r rc
       | BREAK => "break"
       | INFO (w, ra) => "info " ^ (Word32.fmt StringCvt.HEX w) ^ " " ^ r ra
       | RDTSC (ra) => "rdtsc " ^ r ra
       | COMMENT s => "(* " ^ s ^ " *)"

       | UPD_LOCAL  (ra, rb, rc) => "upd_local " ^ r ra ^ "[" ^ r rb ^ "] <- " ^ r rc
       | LOADPROG_LOCAL (ra, rb) => "loadprog_local " ^ r ra ^ " @ " ^ r rb

       | JNZ (rt, v, ra, rb) => "jnz " ^ r rt ^ " " ^ Variable.tostring v ^ 
                                    " (X " ^ r ra ^ " " ^ r rb ^ ")"
       | JZ (rt, v, ra, rb)  => "jz " ^ r rt ^ " " ^ Variable.tostring v ^ 
                                    " (X " ^ r ra ^ " " ^ r rb ^ ")"
       | JLZ (rt, v, ra, rb)  => "jlz " ^ r rt ^ " " ^ Variable.tostring v ^ 
                                    " (X " ^ r ra ^ " " ^ r rb ^ ")"
       | LITERAL_ADDR (ra, v) => "literal_addr " ^ r ra ^ " <- " ^ 
                                      Variable.tostring v
       | LITERAL_ANY (ra, w, rb) => "literal_any " ^ r ra ^ " <- " ^ 
                                      Word32.fmt StringCvt.HEX w ^ " (X " ^ r rb ^ ")"
       | SUB (ra, rb, rc, rd) => "sub " ^ r ra ^ " <- " ^ r rb ^ " - " ^ r rc ^
                                      "(X " ^ r rd ^ ")"

       | OR (ra, rb, rc, rd) => "or " ^ r ra ^ " <- " ^ r rb ^ " | " ^ r rc ^
                                      "(X " ^ r rd ^ ")"

       | AND (ra, rb, rc) => "and " ^ r ra ^ " <- " ^ r rb ^ " & " ^ r rc
       | XOR (ra, rb, rc, rd) => "xor " ^ r ra ^ " <- " ^ r rb ^ " ^ " ^ r rc ^
                                      "(X " ^ r rd ^ ")"

       | NOT (ra, rb) => "not " ^ r ra ^ " <- ~" ^ r rb
       | DEC (ra, rb) => "dec " ^ r ra ^ " <-  (X " ^ r rb ^ ")"
       | DECTO (ra, rb) => "decto " ^ r ra ^ " <- " ^ r rb


       | CALLWITH (ra, v, rb) => "call " ^ Variable.tostring v ^ " (ret: " 
                                      ^ r ra ^ ") (X " ^ r rb ^ ")"

       | MANY l => "MANY:\n " ^ StringUtil.delimit ("\n ") (map intos l) ^ "\n"

       | DATA w => "DATA " ^ Word32.fmt StringCvt.HEX w
       | DATALAB l => "DATALAB " ^ Variable.tostring l
       | LABEL v => "-------- " ^ Variable.tostring v ^ ":"
       | BSSDATA => "BSSDATA"
       | SWAPEND i => "(SWAP-END) " ^ is i)

    in
      enc ^ "   " ^ is i
    end

end

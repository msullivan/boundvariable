
structure UMinUM =
struct

  (* UM interpreter. expects to find the UM code
     immediately following the interpreter. Optimized
     (especially) for size, pulling some tricks.

     By Tom and Spoons

     *)

  open Conventions
  open UMA
  structure V = Variable

  val debug = false

  fun debugsay s =
    (* nb. temporarily uses zero reg, and then restores it *)
    if debug
    then MANY [MANY(emitstring [zz] s),
               (* restore.. *)
               LITERAL(zz, 0w0)]
    else MANY []


  fun code () =
    let
      
      val init = V.namedvar "init"
      val friend = V.namedvar "friend"
      val friendcode = V.namedvar "friendcode"
      val foe = V.namedvar "foe"

      fun err s =  MANY (emitstring [aa, bb, cc, dd, ee, ff, hh] s @
                         [HALT])


      val friend_CMOV = V.namedvar "CMOV"
      val friend_ASUB = V.namedvar "ASUB"
      val friend_UPD = V.namedvar "UPD"
      val friend_ADD = V.namedvar "ADD"
      val friend_MUL = V.namedvar "MUL"
      val friend_DIV = V.namedvar "DIV"
      val friend_NAND = V.namedvar "NAND"
      val friend_HALT = V.namedvar "HALT"
      val friend_ALLOC = V.namedvar "ALLOC"
      val friend_FREE = V.namedvar "FREE"
      val friend_WRITE = V.namedvar "WRITE"
      val friend_READ = V.namedvar "READ"
      val friend_LOADPROG = V.namedvar "LOADPROG"
      val friend_LITERAL = V.namedvar "LITERAL"
      val friend_BAD = V.namedvar "BAD"

      val friend_registers = V.namedvar "registers"
      val friend_jumptable = V.namedvar "jumptable"
      val friend_loadprog_nz = V.namedvar "friend_loadprog_nz"
      val friend_asub_nz = V.namedvar "friend_asub_nz"
      val friend_upd_nz = V.namedvar "friend_upd_nz"

      val lpnz_top1 = V.namedvar "lpnz_top1"
      val lpnz_top2 = V.namedvar "lpnz_top2"

      val lpnz_done1 = V.namedvar "lpnz_done1"
      val lpnz_done2 = V.namedvar "lpnz_done2"

      val const_literalmask = V.namedvar "ltmask"
      val const_literalshift = V.namedvar "ltshift"
      val const_instshift = V.namedvar "inshift"
        
      (* put reg 'A' into dst *)
      fun reg_a dest =
        (* a = bb/64 & 7 *)
        MANY[LITERAL(dest, 0w64),
             DIV(dest, bb, dest),
             AND(dest, cc, dest)]

      fun reg_b dest =
        (* b = bb/8 & 7 *)
        MANY[LITERAL(dest, 0w8),
             DIV(dest, bb, dest),
             AND(dest, cc, dest)]


      fun reg_ab adest bdest tmp =
        MANY[LITERAL(tmp, 0w8),
             DIV(bdest, bb, tmp),
             DIV(adest, bdest, tmp),
             AND(bdest, bdest, cc),
             AND(adest, adest, cc)]

      fun reg_c dest = AND(dest, bb, cc)

       (* if a contains a register number,
          put the contest of that register
          into a. *)
      fun readreg a = ASUB(a, zz, a)
      fun readreg_into dest idx = ASUB(dest, zz, idx)

      fun writereg idx value = UPD(zz, idx, value)

    in
      [
       LABEL init,

       LABEL friend_registers,
       (* this is free. it's actually the aa register,
          but we overwrite it later when we execute
          the literal code on the first entry to the
          loop *)

       (* you've got a friend in pennsylvania *)
       LITERAL_ADDR(ff, friend),

       MANY (List.tabulate(7, fn _ => DATA 0w0)),

       (* for the friend interpreter,
          we reserve the following
          registers.

          aa : execution finger.
          ff : offset of friend label
               (start of interpreter).
          cc : word 0w7.
          zz : word 0w0
          *)

       (* start aa at beginning of object code *)
       LITERAL_ADDR(aa, friendcode),
       LITERAL(cc, 0w7),


       (* the Literal instruction is very common,
          so we save the branch at the bottom of it
          by making it a fallthrough. 
          moreover, these instructions have no effect
          when the registers are set as they are,
          so we don't have to skip over them and we
          save 2 words!!! *)

       LABEL friend_LITERAL,          debugsay "[LIT]",
       (* mask *)
       LITERAL(dd, 0wx2000000 - 0w1),
       (* hh has 1 from below, so we can
          load const_literalshift by adding
          hh to dd in one instruction *)
       ADD(ee, dd, hh),
       AND(dd, bb, dd),
       (* dd is word to store *)
       DIV(bb, bb, ee),
       AND(bb, cc, bb),
       writereg bb dd,
       (* fallthrough... *)

       (* --------- spin cycle! --------- *)
       LABEL friend,

       (* get instruction. it comes from the
          "zero page" *)
       ASUB(bb, zz, aa),

       (* hh will stay live for a bit..*)
       LITERAL(hh, 0w1),
       ADD(aa, hh, aa),

       (* bb : instruction *)

       LITERAL_ADDR(dd, const_instshift),
       ASUB(dd, zz, dd),
       DIV(dd, bb, dd),

       (* dd : opcode *)
       LITERAL_ADDR(ee, friend_jumptable),
       ADD(ee, ee, dd),

       (* get the appropriate opcode label and 
          jump to it *)
       ASUB(ee, zz, ee),
       LOADPROG(zz, ee),

       LABEL friend_jumptable,
       DATALAB friend_CMOV,
       DATALAB friend_ASUB,
       DATALAB friend_UPD,
       DATALAB friend_ADD,
       DATALAB friend_MUL,
       DATALAB friend_DIV,
       DATALAB friend_NAND,
       DATALAB friend_HALT,
       DATALAB friend_ALLOC,
       DATALAB friend_FREE,
       DATALAB friend_WRITE,
       DATALAB friend_READ,
       DATALAB friend_LOADPROG,
       DATALAB friend_LITERAL,
       (* might as well do error checking,
          since it's more or less free.. *)
(*
       DATALAB friend_BAD,
       DATALAB friend_BAD,
       DATALAB friend_BAD,
*)

       (* for instructions,
          aa : current IP.
          bb : whole instruction word.
          cc : registers.
          ff : ----

          dd, ee, hh : free
          *)

       LABEL friend_CMOV,             debugsay "[CM]",
       reg_ab dd ee hh,
       reg_c hh,      readreg hh,
       readreg_into bb dd, readreg ee,

       (* if hh is nonzero *)
       CMOV(bb, ee, hh),
       writereg dd bb,
       LOADPROG(zz, ff),



       LABEL friend_ASUB,
       reg_b dd, readreg dd,
       JNZ (dd, friend_asub_nz, ee, hh),
       (* it's zero! *)
       reg_c dd, readreg dd,
       LITERAL_ADDR(hh, friendcode),
       ADD(dd, hh, dd),
       ASUB(dd, zz, dd),

       reg_a ee,
       writereg ee dd,
       LOADPROG(zz, ff),
       

       LABEL friend_asub_nz,
       (* not too bad... *)
       (* dd : array to subscript *)
       reg_c ee, readreg ee,
       LITERAL(hh, 0w1),
       ADD(ee, hh, ee),
       (* ee : actual offset *)
       ASUB(dd, dd, ee),
       reg_a ee,
       writereg ee dd,
       LOADPROG(zz, ff),


       LABEL friend_UPD,
       reg_a dd, readreg dd,
       JNZ (dd, friend_upd_nz, ee, hh),
       (* it's zero! *)
       reg_c dd, readreg dd,
       (* dd: value to write *)
       reg_b ee, readreg ee,
       LITERAL_ADDR(hh, friendcode),
       ADD(ee, hh, ee),
       (* ee is real offset into 0 array *)
       UPD(zz, ee, dd),
       LOADPROG(zz, ff),

       LABEL friend_upd_nz,
       (* dd : array to amend *)
       reg_b ee, readreg ee,
       LITERAL(hh, 0w1),
       ADD(ee, hh, ee),
       (* ee : actual offset. *)
       reg_c hh, readreg hh,
       UPD(dd, ee, hh),
       LOADPROG(zz, ff),


       LABEL friend_ADD,              debugsay "[ADD]",
       reg_ab dd ee hh, readreg ee,
       reg_c hh,      readreg hh,
       ADD(ee, ee, hh),
       writereg dd ee,
       LOADPROG(zz, ff),

       LABEL friend_MUL,              debugsay "[MUL]",
       reg_ab dd ee hh, readreg ee,
       reg_c hh, readreg hh,
       MUL(ee, ee, hh),
       writereg dd ee,
       LOADPROG(zz, ff),

       LABEL friend_DIV,              debugsay "[DIV]",
       reg_ab dd ee hh, readreg ee,
       reg_c hh,      readreg hh,
       DIV(ee, ee, hh),
       writereg dd ee,
       LOADPROG(zz, ff),

       LABEL friend_NAND,              debugsay "[NAND]",
       reg_ab dd ee hh, readreg ee,
       reg_c hh,      readreg hh,
       NAND(ee, ee, hh),
       writereg dd ee,
       LOADPROG(zz, ff),

       LABEL friend_HALT,              debugsay "[HALT]",
       HALT,

       (* store size in first slot *)
       LABEL friend_ALLOC,
       reg_c dd, readreg dd,
       (* hh has 1 from IP++ *)
       ADD(hh, hh, dd),
       ALLOC(ee, hh),
       UPD(ee, zz, dd),
       reg_b dd, writereg dd ee,
       LOADPROG(zz, ff),
           

       LABEL friend_FREE,
       reg_c dd, readreg dd,
       FREE dd,
       LOADPROG(zz, ff),


       LABEL friend_WRITE,
       reg_c ee, readreg ee,
       WRITE ee,
       LOADPROG(zz, ff),


       LABEL friend_READ,
       READ ee,
       reg_c dd, writereg dd ee,
       LOADPROG(zz, ff),


       LABEL friend_LOADPROG,          debugsay "[LP]",
       reg_b ee, readreg ee,
       JNZ (ee, friend_loadprog_nz, aa, dd),
       reg_c aa, readreg aa,
       LITERAL_ADDR(hh, friendcode),
       ADD(aa, aa, hh),
       LOADPROG(zz, ff),

       
       LABEL friend_loadprog_nz,
       (* actually we do not need FOE MODE.
          all we need to do is create a new
          memory image of the interpreter
          concatentated on top of the program
          that we're being asked to load.
          
          this is called FAUX FOE MODE. *)
       
       LITERAL_ADDR(cc, friendcode),
       (* aa : not needed, because we are
               going to reset the pc.
          ff : size of our code.
          ee : the array we should "jump to" 
          cc : registers *)
       
       (* first, allocate the "DONOR X" array.
          this will be the thing we ultimately
          LOADPROG. get the size of the
          thing that the object level is trying
          to loadprog. *)
       
       ASUB(ff, ee, zz),
       (* dd: size *)
       ADD(dd, ff, cc),
       (* dd : size + us_size *)
       ALLOC(dd, dd),
       (* dd : donor x array of (size + us_size) *)

       (* dd : donor x,
          ff : words left for uminum header *)

       LABEL lpnz_top1,
       JZ(cc, lpnz_done1, aa, hh),

       DEC(cc, hh),
       
       ASUB(aa, zz, cc),
       UPD(dd, cc, aa),

       LITERAL_ADDR(aa, lpnz_top1),   
       LOADPROG(zz, aa),

       LABEL lpnz_done1,

       (* now, update dd with the
          array ee, starting at friendcode. *)
       
       (* ASUB(ff, ee, zz), *)

       (* live: dd (dest), ee (src),
          bb : instruction
          cc : 0w7
          ff : index
          dead : aa, hh
          *)
       LABEL lpnz_top2,
       
       JZ(ff, lpnz_done2, aa, cc),
      
       (* don't decrement, because ff is off by one.
          (size field) *)

       ASUB(aa, ee, ff),

       (* aa holds the word to copy *)

       (* we can go ahead and decrement to be
          0-based again. *)
       DEC(ff, hh),

       (* BUT, we are writing into a cell that's
          offset by the size of the uminum code. *)
       LITERAL_ADDR(hh, friendcode),
       ADD(hh, ff, hh),

       (* hh holds destination for dd *)
       
       UPD(dd, hh, aa),

       (* ok.. *)

       LITERAL_ADDR(aa, lpnz_top2),
       LOADPROG(zz, aa),

       LABEL lpnz_done2,


       (* need to set up invariants for friend. 

          aa: object level instruction pointer
              (C from instruction word)
          ff: pointer to friend label
          *)
       (* restore this before we do reg stuff! *)
       LITERAL(cc, 0w7),
       reg_c aa, readreg aa,
       (* BREAK, *)
       (* LITERAL_ADDR(hh, friendcode), *)
       ADD(aa, aa, hh),
       LITERAL_ADDR(ff, friend),


       LOADPROG(dd, ff),

       LABEL const_instshift,
       DATA (0wx10000000),

       (* at the very end, we label what will
          come next... *)
       LABEL friendcode
       ]
    end


end

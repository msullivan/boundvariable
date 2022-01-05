structure Puzzles
=
struct
    val array = Array.array
    val array_appi = Array.appi
    val array_foldl = Array.foldl
    val sub = Array.sub
    val update = Array.update
    fun shl (x, y) = Word.toInt (Word.<< (Word.fromInt x, Word.fromInt y))
    fun andb (x, y) = Word.toInt (Word.andb (Word.fromInt x, Word.fromInt y))
    infix shl andb

    fun for lo hi f =
        if lo > hi then ()
        else (f lo; for (lo + 1) hi f)
    fun ford lo hi b f =
        if lo > hi then b
        else (ford (lo + 1) hi (f (lo, b)) f)
    type puzzle = { name : string
                  , mem : unit -> int array
                  , reg : unit -> int array
                  , checkmem : int array -> bool
                  , checkreg : int array -> bool }
    (* BEGIN puzzles.uh *)
    fun zeromem () = array (256, 0)
    fun zeroreg () = array (6, 0)
    fun id a (i, _) = update (a, i, i)
    fun idxmem () = let val m = zeromem ()
                        val () = array_appi (id m) m
                    in m
                    end
    fun idxreg () = let val r = zeroreg ()
                        val () = array_appi (id r) r
                    in r
                    end
    (* there exists lo <= i <= hi such that f i *)
    fun exists lo hi f =
        ford lo hi false (fn (i, b) => b orelse f i)
    (* there exist 0 <= i < j <= hi such that f i j *)
    fun exist lo hi f =
        ford lo hi false
             (fn (i, b) =>
                 b orelse ford (i + 1) hi false
                               (fn (j, b) =>
                                   b orelse f i j))
    (* for every lo <= i <= hi, f i *)
    fun all lo hi f =
        ford lo hi true (fn (i, b) => b andalso f i)
    fun vacuous _ = true
    (* XXX seed with hash of input, return different values on each call *)
    fun pick lo hi = lo
    (* XXX update/generate files from puzzles.sml, update umix.uml with filenames *)
    val puzzles =
        { name = "stop"
        , mem = fn () => let val m = zeromem ()
                             val () = update (m, 1, 1)
                         in m
                         end
        , reg = idxreg
        , checkmem = vacuous
        , checkreg = vacuous }

        ::

        { name = "stop0"
        , mem = zeromem
        , reg = zeroreg
        , checkmem = vacuous
        , checkreg = vacuous }

        ::

        { name = "stop1"
        , mem = fn () => let val m = zeromem ()
                             val () = update (m, 1, 1)
                         in m
                         end
        , reg = zeroreg
        , checkmem = vacuous
        , checkreg = vacuous }

        ::

        { name = "stop127"
        , mem = fn () => let val m = zeromem ()
                             val () = update (m, 127, 127)
                         in m
                         end
        , reg = zeroreg
        , checkmem = vacuous
        , checkreg = vacuous }

        ::

        { name = "stop128"
        , mem = fn () => let val m = zeromem ()
                             val () = update (m, 128, 128)
                         in m
                         end
        , reg = zeroreg
        , checkmem = vacuous
        , checkreg = vacuous }

        ::

        let val a = pick 1 255
        in { name = "copymem"
           , mem = fn () => let val m = zeromem ()
                                val () = update (m, 0, a)
                            in m
                            end
           , reg = zeroreg
           , checkmem = vacuous
           , checkreg = fn r => exists 0 5 (fn i => sub (r, i) = a) }
        end

        ::

        let val a = pick 1 255
        in { name = "copymem2"
           , mem = fn () => let val m = zeromem ()
                                val () = update (m, 0, a)
                                val () = update (m, 1, 1)
                            in m
                            end
           , reg = zeroreg
           , checkmem = vacuous
           , checkreg = fn r => exists 0 5 (fn i => sub (r, i) = a) }
        end

        ::

        let val a = pick 1 255
        in { name = "copyreg"
           , mem = fn () => let val m = zeromem ()
                                val () = for 0 7 (fn i => update (m, i, 1 shl i))
                            in m
                            end
           , reg = fn () => let val r = zeroreg ()
                                val () = update (r, 0, a)
                                val () = for 0 4 (fn i => update (r, i + 1, i))
                            in r
                            end
           , checkmem = fn m => exists 0 255 (fn i => sub (m, i) = a)
           , checkreg = vacuous }
        end

        ::

        { name = "swapmem"
        , mem = fn () => let val m = zeromem ()
                             val () = for 0 7 (fn i => update (m, i, 1 shl i))
                         in m
                         end
        , reg = idxreg
        , checkmem = fn r => exist 0 7 (fn i => fn j => sub (r, i) = (1 shl j) andalso sub (r, j) = (1 shl i))
        , checkreg = vacuous }

        ::

        { name = "swapreg"
        , mem = fn () => array (256, 1)
        , reg = idxreg
        , checkmem = vacuous
        , checkreg = fn r => exist 0 5 (fn i => fn j => sub (r, i) = j andalso sub (r, j) = i) }

        ::

        let val abcdxy = array (6, 0)
            val () = for 0 5 (fn i => update (abcdxy, i, pick 1 255))
        in { name = "swapreg2"
           , mem = fn () => array (256, 1)
           , reg = fn () => let val r = zeroreg ()
                                val () = for 0 5 (fn i => update (r, i, sub (abcdxy, i)))
                            in r
                            end
           , checkmem = vacuous
           , checkreg = fn r => exist 0 5 (fn i => fn j => sub (r, i) = sub (abcdxy, j) andalso sub (r, j) = sub (abcdxy, i)) }
        end

        ::

        let val a = pick 1 255
            val b = pick 1 255
        in { name = "addmem"
           , mem = fn () => let val m = zeromem ()
                                val () = update (m, 0, a)
                                val () = update (m, 1, b)
                            in m
                            end
           , reg = idxreg
           , checkmem = fn m => sub (m, 2) = (a + b) mod 256
           , checkreg = vacuous }
        end

        ::

        let val a = pick 1 255
            val b = pick 1 255
        in { name = "addonly"
           , mem = fn () => let val m = zeromem ()
                                val () = update (m, 0, a)
                                val () = update (m, 1, b)
                            in m
                            end
           , reg = idxreg
           , checkmem = fn m => sub (m, 0) = a
                                andalso sub (m, 1) = b
                                andalso sub (m, 2) = ((a + b) andb 0xff)
                                andalso all 3 255 (fn i => sub (m, i) = 0)
           , checkreg = vacuous }
        end

        ::

        let val a = pick 1 255
            val b = pick 1 255
        in { name = "multmem"
           , mem = fn () => let val m = zeromem ()
                                val () = update (m, 0, a)
                                val () = update (m, 1, b)
                            in m
                            end
           , reg = idxreg
           , checkmem = fn m => sub (m, 2) = (a * b) mod 256
           , checkreg = vacuous }
        end

        ::

        let val a = pick 1 255
            val b = pick 1 255
        in { name = "multmem2"
           , mem = fn () => let val m = zeromem ()
                                val () = update (m, 0, a)
                                val () = update (m, 1, b)
                            in m
                            end
           , reg = zeroreg
           , checkmem = fn m => sub (m, 0) = a
                                andalso sub (m, 1) = b
                                andalso sub (m, 2) = ((a * b) andb 0xff)
                                andalso all 3 255 (fn i => sub (m, i) = 0)
           , checkreg = vacuous }
        end

        ::

        let val a = pick 1 255
            val i = pick 8 254
            val j = pick i 255
        in { name = "fillmem"
           , mem = fn () => let val m = zeromem ()
                                val () = update (m, 0, a)
                                val () = update (m, 1, i)
                                val () = update (m, 2, j)
                                (*val () = update (m, 3, 0)*)
                                val () = for 0 3 (fn i => update (m, 4 + i, 1 shl i))
                            in m
                            end
           , reg = idxreg
           , checkmem = fn m => all 8 (i - 1) (fn i => sub (m, i) = 0)
                                andalso all i (j - 1) (fn v => sub (m, i) = a)
                                andalso all j 255 (fn v => sub (m, i) = 0)
           , checkreg = vacuous }
        end

        ::

        { name = "clearreg"
        , mem = idxmem
        , reg = idxreg
        , checkmem = vacuous
        , checkreg = fn r => all 0 255 (fn i => sub (r, i) = 0) }

        ::

        nil
    (* END puzzles.uh *)

    fun prt ({name, mem, reg, ...} : puzzle) =
        let fun prt' s a = Array.app (fn v => (TextIO.output (s, StringUtil.bytetohex v))) a
            val mem = mem ()
            val reg = reg ()
            val s = TextIO.openOut (name ^ ".mem")
            val () = prt' s mem
            val () = TextIO.closeOut s
            val s = TextIO.openOut (name ^ ".srcreg")
            val sreg = Array.tabulate (4, fn i => Array.sub (reg, i))
            val () = prt' s sreg
            val () = TextIO.closeOut s
            val s = TextIO.openOut (name ^ ".dstreg")
            val dreg = Array.tabulate (2, fn i => Array.sub (reg, 4 + i))
            val () = prt' s dreg
            val () = TextIO.closeOut s
        in ()
        end

    fun prtall () = List.app prt puzzles

end

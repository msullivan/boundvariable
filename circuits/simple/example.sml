structure Example =
struct

  open Circuits
       
(*
   *..............................|....................................*
   :plus                          |                                    :
   :  +----SPLIT--+               |                                    :
   -->|send L to D|---------------X--------------------+               :
   :  |send L to R|               |                    |               :
   :  +-----------+               |                    |               :
   :         |                    v                    v               :
   :         |                 +------CASE---+     +---ZERO----+       : 
   :         |                 |case T of R D|---->|send T to R|-------->
   :         |                 +-------------+     +-----------+       : 
   :         |                        |                                : 
   :         |                        v                                : 
   :         |                    +--REC---+       +---SUCC---------+  : 
   :         +------------------->|use plus|------>|send inl(L) to R|--->
   :                              +--------+       +----------------+  :
   *...................................................................*
*) 
  infix ==> 
  fun a ==> b = (a, b)

  val plus : module =  
    let
      val self = "plus"
      val boxes =
        [("CASE",  Case  (Face N, E, S)),
         ("ZERO",  Send1 (Face N, E)),
         ("REC",   Use self),
         ("SUCC",  Send1 (Inr (Face W), E)),
         ("SPLIT", Send2 (Face W, S, Face W, E))]
      val wires =
        [("SPLIT", E) ==> ("ZERO", N),
         ("SPLIT", S) ==> ("REC", W),
         ("CASE", E)  ==> ("ZERO", W),
         ("CASE", S)  ==> ("REC", N),
         ("REC",  E)  ==> ("SUCC", W)]

      val north = SOME ("CASE", N)
      val west  = SOME ("SPLIT", W)
      val east  =
        [("ZERO", E),
         ("SUCC", E)]
    in
      (self, boxes, wires, north, west, east)
    end


(*
  ,..............................|.....................................,
  :mult                          |                                     :
  :  *======SPLIT=======*        |                                     :
  -->!send [(W,E),(W,S)]!--------#-----------+                         :
  :  *==================*        |           |                         :
  :           |                  |           |                         :
  :           |                  v           |    *====ZERO=========*  :
  :           |          *======CASE====*    |  +>!send [(Inl (),E)]!---
  :           |          !case N of E, S!----#--+ *=================*  :
  :           |          *==============*    |                         :
  :           |                  |           |                         :
  :           |                  v           v                         :
  :           |            *===REC==*      *==ADD===*                  :
  :           +----------->!use mult!----->!use plus!-------------------
  :                        *========*      *========*                  :
  :                                                                    :
  ,....................................................................,
*)
  val mult : module = 
    let
      val plus = "plus"
      val self = "mult"
      val boxes =
        [("REC",   Use self),
         ("SPLIT", Send2 (Face W, E, Face W, S)),
         ("CASE",  Case (Face N, E, S)),
         ("ADD",   Use plus),
         ("ZERO",  Send1 (Inl Unit, E))]
      val wires =
        [("SPLIT", E) ==> ("ADD", N),
         ("SPLIT", S) ==> ("REC", W),
         ("CASE", S)  ==> ("REC", N),
         ("CASE", E)  ==> ("ZERO", W),
         ("REC", E)   ==> ("ADD", W)]
         
      val north = SOME ("CASE", N)
      val west  = SOME ("SPLIT", W)
      val east  =
        [("ZERO", E),
         ("ADD", E)]
    in
      (self, boxes, wires, north, west, east)
    end

(*
 ,..............................................................,
 :main                                                          :
 :                                                              :
 :  *====SOURCE======================================*          :
 :  !send [(Inr Inr Inr Inl (),E),(Inr Inr Inl (),S)]!--+       :
 :  *================================================*  |       :
 :                   |                                  v       :
 :                   |                            *=MUL====*    :
 :                   +--------------------------->!use mult!-----
 :                                                *========*    :
 ,..............................................................,
*)
  val main : module = 
    let
      val plus = "plus"
      val mult = "mult"
      val self = "main"
      val boxes =
        [("SOURCE", Send2 (Inr (Inr (Inr (Inl Unit))), E,
                           Inr (Inr (Inl Unit)), S)),
         ("MUL",   Use mult)]
      val wires =
        [("SOURCE", E) ==> ("MUL", N),
         ("SOURCE", S) ==> ("MUL", W)]
         
      val north = NONE
      val west  = NONE
      val east  =
        [("MUL", E)]
    in
      (self, boxes, wires, north, west, east)
    end


end

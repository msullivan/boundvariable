(* Humlock runtime. *)
structure Runtime =
struct

  val lab_pow2_table = Variable.namedvar "pow2_table"
  val pow2tab =
    (lab_pow2_table,
     List.tabulate
     (32,
      fn i => UMA.DATA (Word32.<<(0w1, Word.fromInt i))))

  val lab_shifttable = Variable.namedvar "shift_table"
  val shifttable =
    (lab_shifttable,
     [UMA.DATA 0w1,
      UMA.DATA 0w256,
      UMA.DATA (0w256 * 0w256),
      UMA.DATA (0w256 * 0w256 * 0w256)])

end

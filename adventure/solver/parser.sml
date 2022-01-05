structure Parser =
struct
 open Solver

 datatype item =
     Item of string * string * string list * cond * item option
 and cond =
     Pristine
   | Broken of cond * kind list
 and kind =
     Kind of string * cond

 fun id x = x

 fun success ((Item (name, _, adjectives, c, pile))::rest) =
     let fun convert_cond last (Pristine) = Noun last
           | convert_cond last (Broken (cod, dom)) = Missing (convert_cond last cod,
                                                              List.map convert_kind dom)
         and convert_kind (Kind (name, c)) = convert_cond name c
     in
       ((convert_cond name c, adjectives)
        ::
        (case pile of SOME i => (success (i::nil))
                    | NONE => nil))
       @
       (success rest)
     end
   | success nil = nil

 val command = id
 val look = id
 val go = id
 val show = id
 fun room name description items = items
 val name = id
 val description = id
 val items = id

 fun item name description adjectives condition nil = 
     Item (name, description, adjectives, condition, NONE)
   | item name description adjectives condition (item::nil) =
     Item (name, description, adjectives, condition, SOME item)
   | item _ _ _ _ _ = raise Match

 val adjectives = id
 val adjective = id
 val condition = id
 fun pristine _ = Pristine
 fun broken cod dom = Broken (cod, dom)
 fun kind name condition = Kind (name, condition)
 val missing = id
 val piled_on = id
 val redacted = "[REDACTED]"
 
end

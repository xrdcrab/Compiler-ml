signature FRAME =
sig
    type register = string

    val registers : register list

    val wordSize : int

    val FP : Temp.temp
    val RV : Temp.temp		(* as seen by callee *)

    val tempMap : register Temp.Table.table
    val externalCall : string * Tree.exp list -> Tree.exp

    type frame

    val mainFrame : frame

    datatype access = InFrame of int
                    | InReg of Temp.temp

    val name : frame -> Temp.label

    val newFrame : {name : Temp.label, formals : bool list} -> frame
    val formals : frame -> access list
    val allocLocal : frame -> bool -> access

    val exp : access -> Tree.exp -> Tree.exp

    val staticLink : frame -> access

    val procEntryExit1 : frame * Tree.stm -> Tree.stm

    val procEntryExit2 : frame * Assem.instr list -> Assem.instr list
    val procEntryExit3 : frame * Assem.instr list -> {prolog : string, body : Assem.instr list, epilog : string}

    val string : Temp.label * string -> string

end

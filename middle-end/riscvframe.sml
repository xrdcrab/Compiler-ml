structure RiscVFrame : FRAME =
struct 
  structure M = Temp
  structure R = Tree

  type register = string

  (* fix these ... they are clearly insufficient *)
  val wordSize = 0
  val FP = M.newtemp ()
  val SP = M.newtemp ()
  val RV = M.newtemp ()

  fun externalCall (_, _) = R.CONST 0

  datatype access = InFrame of int
                  | InReg of Temp.temp

  type frame = { name : M.label
               , formals : access list
               , sp : int ref
               }

  fun name _ = M.newlabel ()
  fun newFrame { name, formals } = { name=name, formals=[], sp=ref 0 }
  fun formals _ = []
  fun allocLocal _ _ = InFrame 0
  fun exp _ _ = R.CONST 0
  fun staticLink _ = InFrame 0
  fun procEntryExit1 (_, _) = R.EXP (R.CONST 0)

  datatype frag = PROC of { body : Tree.stm
                          , frame : frame }
                | STRING of Temp.label * string

end

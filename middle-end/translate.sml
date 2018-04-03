signature TRANSLATE =
sig
    structure Frame : FRAME

    type level
    type access			(* not the same as FRAME.access *)

    val outermost : level

    val newLevel : { parent : level
                   , name : Temp.label
                   , formals : bool list } -> level

    val formals : level -> access list
    val allocLocal : level -> bool -> access

    datatype exp = Ex of Tree.exp
                 | Nx of Tree.stm
                 | Cx of Temp.label * Temp.label -> Tree.stm


    datatype frag = PROC of { body : Tree.stm
                            , frame : Frame.frame }
                  | STRING of Temp.label * string

    val initResult : unit -> unit
    val getResult : unit -> frag list
end

structure Translate : TRANSLATE =
struct
  structure A = Absyn
  structure E = ErrorMsg
  structure Frame = RiscVFrame
  structure F = RiscVFrame
  structure M = Temp
  structure R = Tree

  (* fix these ... they are clearly insufficient *)

  type level = unit
  type access = unit

  val outermost = ()
  fun newLevel { parent, name, formals } = ()
  fun formals _ = []
  fun allocLocal _ _ = ()

    datatype exp = Ex of R.exp
                 | Nx of R.stm
                 | Cx of M.label * M.label -> R.stm


    datatype frag = PROC of { body : R.stm
                            , frame : F.frame }
                  | STRING of Temp.label * string

    fun initResult _ = ()
    fun getResult _ = []
end

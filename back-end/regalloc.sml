signature REG_ALLOC =
sig
  structure Frame : FRAME
  type allocation = Frame.register Temp.Table.table
  val alloc : Assem.instr list * Frame.frame -> Assem.instr list * allocation
end

structure RegAlloc : REG_ALLOC =
struct
  structure Frame = RiscVFrame

  structure A = Assem
  structure E = ErrorMsg
  structure F = RiscVFrame
  structure M = Temp

  type allocation = F.register M.Table.table

  fun alloc (insts, frame) = ([], M.Table.empty)
end

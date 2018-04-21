signature REG_ALLOC =
sig
  structure Frame : FRAME

  type allocation = Frame.register Temp.Table.table
  val alloc : Assem.instr list * Frame.frame -> Assem.instr list * allocation
  val instrs2graph : Assem.instr list -> Flow.flowgraph * Graph.node list
end

structure RegAlloc : REG_ALLOC =
struct
  structure Frame = RiscVFrame

  structure A = Assem
  structure E = ErrorMsg
  structure F = RiscVFrame
  structure M = Temp


  type allocation = F.register M.Table.table

   fun instrs2graph instrs = 


      E.impossible "you need to implement instrs3graph"

  fun alloc (instrs, frame) =
    let
      val (flowgraph,nodes) = instrs2graph(instrs)
      val (igraph, liveMap) = Liveness.interferenceGraph(flowgraph)
      val (allocation, listSpills) = Color.color({interference = igraph, initial = F.tempMap, spillCost = fn _ => 1, registers = F.registers})
    in
      (instrs, allocation)
    end




end

structure MakeGraph :
  sig
    val instrs2graph : Assem.instr list -> Flow.flowgraph * Graph.node list
  end

= struct
    structure E = ErrorMsg
    structure W = Flow
    structure G = Graph

    fun instrs2graph _ = E.impossible "you need to implement instrs3graph"
end

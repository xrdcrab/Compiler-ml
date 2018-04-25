signature REG_ALLOC =
sig
  structure Frame : FRAME

  type allocation = Frame.register Temp.Table.table
  val alloc : Assem.instr list * Frame.frame -> Assem.instr list * allocation
  val instrs2graph : Assem.instr list -> Flow.flowgraph * Flow.Graph.node list
end

structure RegAlloc : REG_ALLOC =
struct
  structure Frame = RiscVFrame

  structure A = Assem
  structure E = ErrorMsg
  structure F = RiscVFrame
  structure M = Temp
  structure W = Flow
  structure G = Graph

  structure WG = W.Graph
  structure GT = G.Table


  type allocation = F.register M.Table.table

  (* helper functions *)
   fun makeNodes (instr::instrs, cG) = G.newNode(cG)::makeNodes(instrs, cG)
     | makeNodes ([], cG) = []

   fun findNode (node::nodes, instr_id, i) = 
         if instr_id = i 
         then node 
         else findNode(nodes, instr_id, i+1)
     | findNode ([], _, _) = E.impossible "Shouldn't be empty."

    fun makeJumpEdge (curNode, nodes, pairs, labels) =
        let 
            fun addJumpEdge (curNode, nodes, (l, n)::pairs, label) = 
                (if l = label
                 then G.mk_edge({from=curNode, to=findNode(nodes, n, 0)})
                 else ();
                 addJumpEdge(curNode, nodes, pairs, label)
                )
              | addJumpEdge (_, _, [], _) = ()
        in
          (case labels of
            [] => ()
          | label::xs => 
            (addJumpEdge(curNode, nodes, pairs, label);
             makeJumpEdge(curNode, nodes, pairs, xs)))
        end

   fun makePairs (instrs, instr_id) = 
       case instrs of 
         A.LABEL{assem=a, lab=label}::xs => (label, instr_id)::makePairs(xs, instr_id+1)
       | [] => []
       | _::xs => makePairs(xs, instr_id+1)

   
   fun instrs2graph instrs = 
      let 
        val g = G.newGraph();
        val flowG = W.FGRAPH{control = g, def = GT.empty, use = GT.empty, ismove = GT.empty};
        val nodes = makeNodes(instrs, g);
        val pairs = makePairs(instrs, 0);
        fun makeFlowGraph(instrs, flowG as W.FGRAPH{control, def, use, ismove}, nodes, instr_id, pairs) = 
         case instrs of 
           A.OPER{assem=a, dst=defs, src=srcs, jump=SOME(labelList)}::xs =>
             (let 
                val curNode = findNode(nodes, instr_id, 0);
                val newDefs = GT.enter(def, curNode, defs);
                val newUses = GT.enter(use, curNode, srcs);
                val newIsMoves = GT.enter(ismove, curNode, false);
              in(
                makeJumpEdge(curNode, nodes, pairs, labelList);
                makeFlowGraph(xs, W.FGRAPH{control=control, def=newDefs, use=newUses, ismove=newIsMoves}, nodes, instr_id+1, pairs)
              )  
              end)
         | A.OPER{assem=a, dst=defs, src=srcs, jump=NONE}::xs =>
             (let
                val curNode = findNode(nodes, instr_id, 0);
                val newDefs = GT.enter(def, curNode, defs);
                val newUses = GT.enter(use, curNode, srcs);
                val newIsMoves = GT.enter(ismove, curNode, false);
              in(
                if xs = [] 
                then ()
                else (G.mk_edge({from=curNode, to=findNode (nodes, instr_id+1, 0)}); ());
                makeFlowGraph(xs, W.FGRAPH{control=control, def=newDefs, use=newUses, ismove=newIsMoves}, nodes, instr_id+1, pairs)
              ) 
              end)
         | A.LABEL{assem=a, lab=label}::xs =>
             (if xs = [] then makeFlowGraph(xs, flowG, nodes, instr_id+1, pairs)
              else
              (G.mk_edge({from=findNode(nodes, instr_id, 0),
                           to=findNode(nodes, instr_id+1, 0)}); 
                           makeFlowGraph(xs, flowG, nodes, instr_id+1, pairs)))
         | A.MOVE{assem=a, dst=dsts, src=srcs}::xs =>
             (let 
                val curNode = findNode(nodes, instr_id, 0);
                val newDefs = GT.enter(def, curNode, dsts::[]);
                val newUses = GT.enter(use, curNode, srcs::[]);
                val newIsMoves = GT.enter(ismove, curNode, true);
              in(
                if xs = []
                then ()
                else (G.mk_edge({from=curNode, to=findNode(nodes, instr_id+1, 0)}); ());
                makeFlowGraph(xs, W.FGRAPH{control=control, def=newDefs, use=newUses, ismove=newIsMoves}, nodes, instr_id+1, pairs) 
              ) 
              end)
         | [] => (flowG, nodes)
      in
        makeFlowGraph(instrs, flowG, nodes, 0, pairs)
      end


  fun alloc (instrs, frame) =
    let
      val (flowgraph,nodes) = instrs2graph(instrs)
      val (igraph, liveMap) = Liveness.interferenceGraph(flowgraph, nodes)

    (*  val _ = Liveness.show(TextIO.stdOut,igraph) *)
      val (allocation, _) = Color.color({interference = igraph, initial = Frame.tempMap, spillCost = fn _ => 1, registers = Frame.registers})

    in
      (instrs, allocation)
    end




end

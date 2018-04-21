signature COLOR =
sig
  structure Frame : FRAME

  type allocation = Frame.register Temp.Table.table

  val color : { interference : Liveness.igraph
              ,      initial : allocation
              ,    spillCost : Graph.node -> int 
              ,    registers : Frame.register list
              } -> allocation * Temp.temp list
end

structure Color : COLOR =
struct
  structure Frame = RiscVFrame

  structure A = Assem
  structure E = ErrorMsg
  structure F = RiscVFrame
  structure G = Graph
  structure M = Temp

  type allocation = F.register M.Table.table

  fun color ({interference, initial, spillCost, registers}) = (M.Table.empty,  [])
   (*) let  *)
      (* machine registers, preassigned a color *)
      val precolored =1
      (* temporary registers, not precolored and not yet processed*)
      val initial =1 
      (* list of low-degree non-move-related nodes.*)
      val simplifyWorklist = 1
      (* low-degree move-related nodes*)
      val freezeWorklist = 1
      (* high-degree nodes*)
      val spillWorklist=1
      (* nodes marked for spilling during this round; initially empty*)
      val spilledNodes=1
      (* registers that have been coalesced; when u <- v is coalesced, v is added to this set and u put back on same work-list. *)
      val coalescedNodes=1
      (* nodes successfully colored *)
      val coloredNodes=1
      (* stack containing tempporaries removed from the graph *)
      val selectStack=1

      (* MOVE SETS, five sets of move instructions, every move is in exactly one of these sets*)
      (* moves that have been coalesced*)
      val coalescedMoves=1
      (* moves whose source and target interfere*)
      val constrainedMoves=1
      (* moves that will no longer be considered for coalescing*)
      val frozenMoves=1
      (* moves enabled for possible coalescing*)
      val worklistMoves=1
      (* moves not yet ready for coalescing*)
      val activeMoves=1

      (* the set of interference edges(u,v) in the graph. If(u,v) belong adjSet then (v,u) belong adjSet*)
      val adjSet=1
      (* adjacency list representation of the graph; for each non-precolored temporary u, adjList[u] is the set of nodes that interfere with u.*)
      val adjList=1
      (* an array containing the current degree of each node*)
      val degree=1
      (* a mapping from a node to the list of moves it is associated with*)
      val moveList=1
      (* when a move(u,v) has been coalesced, and v put in coalescedNodes, then alias(v)=u*)
      val alias=1
      (* the color cosen by the algorithm for a node; for precolored nodes this is initialized to the given color*)
      val color=1
(*)
      fun Build =1

      fun AddEdge=1

      fun MakeWorklist=1

      fun Adjacent=1

      fun NodeMoves=1

      fun MoveRelated=1

      fun Simplify=1

      fun DecrementDegree=1

      fun EnableMoves=1

      fun Coalesce=1

      fun AddWorkList=1

      fun OK=1

      fun Conservative=1

      fun GetAlias=1

      fun Combine=1

      fun Freeze=1

      fun FreezeMoves=1

      fun SelectSpill=1

      fun AssignColors=1

      fun RewriteProgram=1
*)
(*
    in
         Build();
        MakeWorklist();
        AssignColors();
        (allocation,[])
        (allocation,[])
    end
*)

end

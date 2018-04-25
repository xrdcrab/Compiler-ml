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
  structure GT = G.Table
  structure M = Temp
  structure MT = M.Table
  type allocation = F.register MT.table

  structure Set = ListSetFn(
    struct 
      type ord_key = M.temp
      val compare = Int.compare
      end)

  structure RegSet = ListSetFn(
    struct 
      type ord_key = string
      val compare = String.compare
      end)

  (* interference: igraph
     initial: register allocation
     spillCost: not implementing currently 
    registers: list of all machine registers
     *)
  fun color {interference, initial, spillCost, registers} = 
    let 
      (* K color *)
      val K = length(F.registers)
      val Liveness.IGRAPH{graph, tnode, gtemp, moves} = interference

      (* get nodes from interference graph *)
      val nodes = G.nodes (graph)
      val numNodes =(List.length(nodes))

      (* get temporaries from nodes *)
      val temporaries = (map gtemp (nodes))

      (* get degree of node *)
      fun getDegree node = length(G.adj node)

      (* if the node can be removed from igraph, add to workList*)
      fun makeWorklist (node,workList) = 
        if (getDegree node) < K then
          ((* print ("numer of simplifyWorklis\n"^Int.toString(List.length(workList))); *)
          node::workList)
        else(
          (*print "current node has more degree than K, need to spill.....\n"; *)
          workList)

      (* for nodes with degree < k, add to simplifyworklist *)
      val simplifyWorklist = foldr makeWorklist [] nodes

    fun addToSet () =
      let
        (* build adjSet *)
        fun add (node,table) = GT.enter(table,node,G.adj node)
      in 
        (* add each node in igraph into table*)
        (* for each node, add its adjacent nodes list with itself's value *)
        foldl add GT.empty nodes
      end
    val adjSet = addToSet()
    (* get the node's adjacent list *)
    fun getAdjSet node = 
      case GT.look(adjSet,node) of
                SOME(list) => list
              | NONE => (nil)

    (* if node is able to be simplify, add it to stack, remove connection for its adjacent nodes,
     and return the new stack with new simplifyWorklist *)
    fun simplify(node,(selectStack, simplifyWorklist)) = 
      let
        (* decrease degree of node's adjacent nodes by disconnect them to node *)
        fun decDegree ()= 
          let
            (* remove adjacent nodes of n *)
            fun removeAdj(node1) = 
              if (List.exists (fn n2 => G.eq(node,n2)) (G.pred node1)) then
                  G.rm_edge{from=node, to = node1}
              else 
                  G.rm_edge{from=node1, to = node}
            (* iterate through n's adjacentNode list *)
            fun iter (x::xs) =  
              let
              in
                  removeAdj(x); iter(xs)
              end
                |iter ([]) =
                ()
          in
            iter (G.adj(node))
          end

        (* if n can be simplify, add it to selectStack *)
        fun addToStack () = 
          if getDegree node < K then 
            ((*(print ("number of stack: \n" ^ Int.toString(List.length(selectStack)) ^ "\n"));*)
            node::selectStack)
          else selectStack
        fun createSubList() =
          let 
          in 
            foldl makeWorklist simplifyWorklist (Graph.adj node) 
          end
      in
        if getDegree node < K then
          (* in can be simplified, decrese its degree, add to stack, create new sublist*)
          (decDegree ();
          (* print ("length of swl : "^Int.toString(List.length(simplifyWorklist)) ^"\n"); *)
          (addToStack(),createSubList()))
          (* else, this node needs to be spilled, disconnect it from graph, but not add to stack, 
            so SOME(getDegree=K) now becomes K-1, enable more simplify
             *)
        else (decDegree ();
          (selectStack,(createSubList())))
      end

      (* while simplifyWorkList is not empty, keep simplify, return the final stack when finished *)
      fun repeat(selectStack, simplifyWorklist) = 
        let
          val (newStack,subWorkList) = foldl simplify (selectStack,[]) simplifyWorklist
        in
          case subWorkList of 
            [] => newStack
          | _  => repeat(newStack, subWorkList)
        end


      fun assignColors (node,(coloredNodes, remainColors)) =
        let
          val regSet = RegSet.addList(RegSet.empty, registers)

          (* get assignable colors; 
          passed current node's adjacent node, remove color of adjacent nodes;
          colors left are assinable colors *)
          fun getOkColors (node,okColors) = 
            if Set.member(coloredNodes,(gtemp node)) then 
              (if RegSet.member(okColors,valOf(MT.look(remainColors,gtemp node))) then 
                  RegSet.delete(okColors,valOf(MT.look(remainColors,gtemp node))) 
                else 
                  okColors)
            else ( 
              (* adjacent node not assigned color yet*)
              okColors)
            (* find avaliable colors for node, color it, return new coloredNodes and remainingColors*)
          fun getNewState (node) =
            let
              (* remove colors of node's neibours, remanin okColors *)
              val okColors = foldl getOkColors regSet (getAdjSet node)
              (* add to coloredNode *)
              val coloredNodes = Set.add(coloredNodes,gtemp node)
              val color = hd (RegSet.listItems(okColors)) 
              (*  val _ = print ("avaliable colors:"^Int.toString(RegSet.numItems(okColors))^"\n")  *)
              (* add just assigned pair to table *)
              val remainColors = MT.enter(remainColors,gtemp node,color)
            in
                (coloredNodes,remainColors) 
            end
        in
          getNewState node
        end

           (*
      fun printReg (n) =
        print (Liveness.showNode(n,interference) ^ " =====>" ^ valOf(MT.look(F.tempMap,gtemp n)) ^ "\n")
      val _ = map printReg nodes*)

      (* selectStack after simplify *)
      val selectStack = repeat([],simplifyWorklist)
      val _ = print ("size of stack: "^Int.toString(List.length(selectStack)) ^"\n") 
      val (coloredNodes,allocation) = foldl assignColors (Set.empty,initial) selectStack
    in 
      (allocation, [])
    end


end

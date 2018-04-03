structure Liveness :
sig 
    datatype igraph = IGRAPH of {graph : Graph.graph,
				 tnode : Temp.temp -> Graph.node,
				 gtemp : Graph.node -> Temp.temp,
				 moves : (Graph.node * Graph.node) list}
				
    val interferenceGraph : Flow.flowgraph * Flow.Graph.node list -> igraph * (Flow.Graph.node -> Temp.temp list)
					      
    val show : TextIO.outstream * igraph -> unit
end =

struct 

    structure E = ErrorMsg
    structure G = Graph
    structure GB = Graph.Table
    structure M = Temp
    structure MB = M.Table
    structure U = UnparseAbsyn
    structure W = Flow
    structure WG = W.Graph
    structure WGB = WG.Table

    structure ARR = Array
    structure BITS = BitArray
    structure LST = List
    structure OPT = Option
    structure VEC = Vector
		    
    datatype igraph = IGRAPH of { graph : G.graph,
                                  tnode : M.temp -> G.node,
                                  gtemp : G.node -> M.temp,
                                  moves : (G.node * G.node) list}
				
    type liveSet = unit MB.table * M.temp list
    type liveMap = liveSet WGB.table

    fun interferenceGraph (W.FGRAPH {control, def, use, ismove}, fnodes) =
	let 
	(* step 0: build a graph *)
	    val graph = G.newGraph ()

	(* step 1: collect all temporaries yielding the list of unique temps,
	                                            the list of corresponding igraph nodes,
	                                            a temp->(inode, unique int index) table,
                                                    a inode->temp table,
                                                    the number of temps                      *)
	    val (temps, inodes, tempTable, inodeTable, tempCount) = foldr (fn (t, (ts, ns, tT, nT, i)) => case MB.look (tT, t)
													   of NONE => let val n = G.newNode graph
														      in (t::ts,
															  n::ns,
															  MB.enter (tT, t, (n, i)),
															  GB.enter (nT, n, t),
															  i+1)
														      end
													    | SOME _ => (ts, ns, tT, nT, i))
									  ([], [], MB.empty, GB.empty, 0)
									  (foldl (fn (n, ts) => (OPT.getOpt (GB.look (def, n), []))
												@ (OPT.getOpt (GB.look (use, n), []))
												@ ts)
										 []
										 (WG.nodes control))

	    fun tempLook t = case MB.look (tempTable, t)
			      of NONE => E.impossible "LIVENESS: TEMP WITHOUT INODE AND INDEX"
			       | SOME v => v

	    fun tnode t = let val (n, _) = tempLook t
			  in n
			  end

	    fun tempIndex t = let val (_, i) = tempLook t
			      in i
			      end

	    fun gtemp n = case GB.look (inodeTable, n)
			   of NONE => E.impossible "LIVENESS: INODE WITHOUT TEMP"
			    | SOME t => t

	    fun index2temp tis = let fun collectTemps (_,     _, [])    = []
				       | collectTemps (t::ts, i, b::bs) = if (i=b)
									  then t::(collectTemps (ts, i-1, bs))
									  else collectTemps (ts, i-1, b::bs)
				       | collectTemps ([],    _, bs)    = E.impossible ("LIVENESS: MORE BITS THAN TEMPS " ^ U.commaSep (map Int.toString bs))
				 in collectTemps (temps, (length temps)-1, rev tis)
				 end
				 
	    (* step 2: order nodes from exit to start using dfs search across predecessor links *)
	    (* the nodes in ther reverse of the list generated alongside the flowgraph are pretty close *)
			     (*
			      *	let val seen = ref WGB.empty
			      *	    fun enterNode n = case (WGB.look (!seen, n))
			      *			       of NONE    => ( seen := WGB.enter (!seen, n, ())
			      *					     ; foldr (fn (n, ns) => (enterNode n) @ ns)
			      *						     [n]
			      *						     (G.pred n)
			      *					     )
			      *				| SOME () => []
			      *	in foldr (fn (n, ns) => (enterNode n) @ ns)
			      *		 []
			      *		 (WG.nodes control)
			      *	end
			      *)

					
	    (* and build similar table: fnode -> int index
                   and a total number of flownodes         *)
	    val (fnodeTable, fnodeCount) = foldl (fn (n, (ntable, next)) => (WGB.enter (ntable, n, next),
									     next+1))
						 (WGB.empty, 0)
						 fnodes

	    fun fnodeIndex n = OPT.getOpt (WGB.look (fnodeTable, n), ~1) (* error if  then not found *)
	
	    (* and a vector containing a list of the successor node indicies for each node *)
	    val succIndex = VEC.fromList (map (fn n => map fnodeIndex (G.succ n))
					      fnodes)

	    (* step 3: precompute bits for def and use from each fnode *)
	    fun temps2bits ts = BITS.bits (tempCount, map tempIndex ts)

	    fun bits2temps bs = index2temp (BITS.getBits bs)

	    fun textBits bits = U.commaSep (map M.makestring (bits2temps bits))

	    fun nodes2bits tab = VEC.fromList (map (fn n => temps2bits (OPT.getOpt (WGB.look (tab, n), [])))
						   fnodes)

	    val defnNEG = let val bits = nodes2bits def
			  in ( VEC.app BITS.complement bits
			     ; bits
			     )
			  end

	    val usen = nodes2bits use

	    (* step 4: iterate over the nodes calculating the in[n] and out[n]
		       until no changes are made in an entire iteration         cf. MCI-SML p.214 *)
	    val inn  = ARR.array (fnodeCount, BITS.bits (tempCount, []))
	    val outn = ARR.array (fnodeCount, BITS.bits (tempCount, []))

	    val done = ref false

	in ( while (not (!done)) do
		 ( done := true
		 ; let val n = ref (fnodeCount-1)
		   in while ((!n) >= 0) do
			  ( let val inn' = ARR.sub (inn, !n)
				val outn' = ARR.sub (outn, !n)
			    in ( ARR.update (inn, !n, BITS.orb (VEC.sub (usen, !n),
								BITS.andb (ARR.sub (outn, !n),
									   VEC.sub (defnNEG, !n),
									   tempCount),
								tempCount))
			       ; ARR.update (outn, !n, BITS.bits (tempCount, []))
			       ; app (fn s => ARR.update (outn, !n, BITS.orb (ARR.sub (outn, !n),
									      ARR.sub (inn, s),
									      tempCount)))
				     (VEC.sub (succIndex, !n))
			       (* ; TextIO.print (Int.toString (!n)
					       ^ " outs = "
					       ^ textBits (ARR.sub (outn, !n))
					       ^ "\n") *)
			       ; if (not (BITS.equal (inn', ARR.sub (inn, !n))
					  andalso BITS.equal (outn', ARR.sub (outn, !n))))
				 then done := false
				 else ()
			       )
			    end
			  ; n := !n-1
			  )
		   end
		 )

	   (* step 5: convert to useful (liveMap) form *)
	   ; let val liveMap = foldr (fn (n, liveMap) => let val liveTemps = bits2temps (ARR.sub (outn, fnodeIndex n))
							 in WGB.enter (liveMap,
								       n,
								       ((foldr (fn (t, tt) => MB.enter (tt, t, ()))
									       MB.empty
									       liveTemps),
									liveTemps))
							 end)
				     WGB.empty
				     fnodes

		 fun node2Lives n = case WGB.look (liveMap, n)
				     of NONE         => E.impossible "LIVENESS: NODE NOT ENTERED INTO LIVEOUT"
				      | SOME (_, ts) => ts

		 fun mkEdge (n, n') = if G.eq(n,n')
				      then ()
				      else ( if LST.exists (fn n'' => G.eq(n',n'')) (G.succ n)
					     then ()
					     else G.mk_edge {from=n, to=n'}
					   ; if LST.exists (fn n'' => G.eq(n',n'')) (G.pred n)
					     then ()
					     else G.mk_edge {from=n', to=n}
					   )

		 (* step 6: apply liveMap to build interference graph
			    populate the igraph with interference edges, collecting moves along the way *)
		 val moves = foldr (fn (n, ms) => let val isMove = OPT.getOpt (WGB.look (ismove, n), false)
						  in foldr (fn (d, ms) => foldr (fn (l, ms) => let val dn = tnode d
												   val ln = tnode l
											       in if isMove andalso (case WGB.look (use, n)
														      of SOME [t] => l=t
														       | _ => false)
												  then (dn, ln)::ms
												  else ( mkEdge (dn, ln)
												       ; ms
												       )
											       end)
										ms
										(node2Lives n))
							   ms
							   (OPT.getOpt (WGB.look (def, n), []))
						  end)
				   []
				   fnodes

	     (* done! *)
	     in (IGRAPH {graph=graph,
			 tnode=tnode,
			 gtemp=gtemp,
			 moves=moves},
		 node2Lives)
	     end
	   )
        end

    fun show (stream, IGRAPH {graph, tnode, gtemp, moves}) = let fun showNode n = M.makestring (gtemp n)
							     in ( app (fn n => TextIO.output (stream, "#"
												      ^ showNode n
												      ^ " ===> "
												      ^ U.commaSep (map showNode (G.succ n)) (* pred duplicates this *)
												      ^ "\n"))
								      (G.nodes graph)
								; TextIO.flushOut stream
								)
							     end
end

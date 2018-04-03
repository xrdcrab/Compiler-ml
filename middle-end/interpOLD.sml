(* Interpreter ... should work with an expty but MCIiML fuses type-checking and IR translation *)

structure Interp : 
     sig val exec : TextIO.outstream * Absyn.exp -> unit
     end =
struct

  structure A = Absyn
  structure E = ErrorMsg
  structure P = ListPair
  structure S = Symbol
  structure T = Types
  structure V = Vector

  (* data storage *)
  datatype value = NilV
		 | IntV of int
		 | BoolV of bool
		 | StrV of string
		 | ArrV of cell V.vector
		 | RecV of (S.symbol * cell) list
		 | FunV of S.symbol list * A.exp * env ref (* ref <=> !! recursive functions !! *)
		 | LibV of value list -> cont -> unit
		 | ContV of cont
	         | UnitV

  withtype cell = value ref
			
       and env = S.symbol -> A.pos -> cell (* could use Table ... but its nice to be self-contained *)
				 
       and cont = value -> unit
			       

  fun emptyEnv i pos = E.crashAt pos ((S.name i) ^  " not found.")
				      
  fun extEnv r i v = let val v_ = ref v
		      in fn i' => fn p => if i=i' then v_
			                          else (r i p)
		     end

  fun extEnvL r [] = r
    | extEnvL r ((i,v)::ivs) = extEnv (extEnvL r ivs) i v
				       
  fun findEnv r i p = (r i p)
			  
  fun getEnv r i p = !(findEnv r i p)

  fun setEnv r i v p = (findEnv r i p) := v

  (* interpreter in monadic style *)

  (* the continuation-passing monad
   * K<a> == CONT -> a
   *)
					    
  (* ^^ : a -> K<a>                      LIFT *)
  fun ^^ a k = k a
  (* >>= : K<a> -> (a -> K<b>) -> K<b>   BIND *)
  fun id x = x
  fun >>= (f, g) = g (f id)
  infix >>=
	    
  val breakName = S.symbol "%BREAK%"
  val unscopedBreak = ContV (fn _ => E.crash "break outside loop")

  (* expressions -- evalExp : env -> exp -> K<value> *)
  fun evalExp r (A.VarExp v) = (evalVar r v)
			       >>= (fn v_ => ^^ !v_)
    
    | evalExp _  A.NilExp            = ^^ NilV
    | evalExp _ (A.IntExp i)         = ^^ (IntV i)
    | evalExp _ (A.BoolExp b)        = ^^ (BoolV b)
    | evalExp _ (A.StringExp (s, _)) = ^^ (StrV s)

    | evalExp r (A.CallExp{func,args,pos}) = (case (getEnv r func pos)
					       of (FunV (is, e, r_)) => (evalExps r args)
								        >>= (fn vs => (evalExp (extEnvL (!r_)
													(P.zipEq (is, vs)
													 handle  UnequalLengths => E.crashAt pos "wrong number of arguments to function"))
											       e))
						| _ => E.crashAt pos ("calling " ^ (S.name func) ^ " : not-a-function"))

    | evalExp r (A.OpExp{left,oper,right,pos}) = (evalExp r left)
					         >>= (fn vl => (evalExp r right)
					         >>= (fn vr => ^^ (applyOp oper vl vr pos)))

    | evalExp r (A.RecordExp{fields, typ, pos}) = let fun evalFields ivs [] = ^^ (RecV ivs)
							| evalFields ivs ((i,e,p)::ieps) = (evalExp r e)
											   >>= (fn v => evalFields ((i, ref v)::ivs) ieps)
						   in evalFields [] fields
						  end
										
    | evalExp r (A.SeqExp [])      = ( E.warn "empty sequence"
				     ; ^^ UnitV
				     )
    | evalExp r (A.SeqExp [(e,p)]) = ( E.warnAt p "singleton sequence"
		 		     ; evalExp r e
				     )
    | evalExp r (A.SeqExp eps)     = let fun evalNext v [] = ^^ v
					   | evalNext _ ((e,_)::eps) = (evalExp r e)
						 		       >>= (fn v => (evalNext v eps))
				      in evalNext UnitV eps
				     end

    | evalExp r (A.AssignExp{var=v,exp,pos}) = (evalVar r v)
					       >>= (fn v_ => (evalExp r exp)
					       >>= (fn v => ( v_ := v
							    ; ^^ UnitV)))
    | evalExp r (A.IfExp{test,then',else',pos}) = (evalExp r test)
						  >>= (fn (BoolV true)  => evalExp r then'
						        | (BoolV false) => (case else'
									     of NONE => ^^ UnitV
									     |  SOME e => evalExp r e)
							| _ => E.crashAt pos "if-test not boolean")

    | evalExp r (A.WhileExp{test,body,pos}) = (fn k => let val r = extEnv r breakName (ContV k)
							   fun loop () = (evalExp r test)
									 >>= (fn (BoolV false) => ^^ UnitV
									       | (BoolV true)  => (evalExp r body)
												  >>= loop ())
						        in (loop () k)
						       end)
								      
    | evalExp r (A.ForExp{var,escape,lo,hi,body,pos}) = (fn k => ((evalExp r lo)
								 >>= (fn (IntV si) => (evalExp r hi)
								 >>= (fn (IntV ei) => let val tst = if (si < ei) then (fn i => i > ei)
													         else (fn i => i < ei)
											  val r = extEnv (extEnv r breakName (ContV k)) var (IntV si)
											  fun loop () = (case (getEnv r var pos)
													  of IntV i => if (tst i) then (evalExp r body)
																       >>= loop ()
														                 else (fn _ => k UnitV)
													| _ => E.crashAt pos "for-loop index not-integer")
										       in loop ()
										      end
								     | _ => E.crashAt pos "for-loop end not-integer")
							             | _ => E.crashAt pos "for-loop start not-integer") k))

    | evalExp r (A.BreakExp pos) = (fn k => (case (getEnv r breakName pos)
					      of (ContV k') => k' UnitV
					       | _ => E.crashAt pos "!!break with not continuation!!") k')

    | evalExp r (A.LetExp{decs,body,pos}) = let fun loop [] r = evalExp r body
						  | loop (d::ds) r = (evalDecl r d)
								     >>= (fn r => loop ds r)
					     in loop decs r
					    end

    | evalExp r (A.ArrayExp{typ,size,init,pos}) = (evalExp r size)
						  >>= (fn (IntV n) => if (n<0) then E.crashAt pos "negative-size array"
								               else (evalExp r init)
									       >>= (fn v => let fun loop 0 vs_ = vs_
												  | loop i vs_ = loop (i-1) ((ref v)::vs_)
											     in ^^ (V.fromList (loop n []))
											    end)
						      | _ => E.crashAt pos "array size not integer")



  (* evalDecl computes a new environment, not a new value *)
  and evalDecl r (A.FunctionDec fds) = let fun pname {name,escape,typ,pos} = name
					   val nfvs = map (fn ({name,params,result,body,pos}) => (name, FunV ((map pname params), body, (ref emptyEnv)))) fds
					   val r = extEnvL r nfvs
				        in ( app (fn (_,FunV (_, _, r_)) => (r_ := r)) nfvs
					   ; ^^ r )
				       end
    | evalDecl r (A.TypeDec _) = ^^ r
    | evalDecl r (A.VarDec {name,escape,typ,init,pos}) = (evalExp r init)
		  					 >>= (fn v => ^^ (extEnv r name v))

  and evalExps r [] = ^^ []
    | evalExps r (ae::aes) = (evalExp r ae)
			     >>= (fn v => (evalExps r aes
			     >>= (fn vs => ^^ (v::vs))))
						      
  (* variables -- evalVar : env -> var -> K<value> *)
  and evalVar r (A.SimpleVar(s, p)) = ^^ (findEnv r s p)
    | evalVar r (A.FieldVar(v, s, p)) = (evalVar r v)
					>>= (fn v_ => case (!v_)
						       of RecV svs => let fun find [] = E.crashAt p ("field " ^ (S.name s) ^ " not found")
									    | find ((s',v_)::svs) = if s'=s then ^^ v_
											                    else find svs
								       in find svs
								      end
						    | _ => E.crashAt p ("field " ^ (S.name s) ^ " for not-a-record"))
    | evalVar r (A.SubscriptVar(v, e, p)) = (evalVar r v)
					    >>= (fn v_ => case !v_
							   of ArrV vs => (evalExp r e)
									 >>= (fn (IntV i) => if (i<0) orelse (i>=(V.length vs))
											     then E.crashAt p ("array index " ^ (Int.toString i) ^ " out-of-bounds")
											     else ^^ (V.sub (vs, i))
									     | _ => E.crashAt p "array index not integer")
							    | _ =>  E.crashAt p "index into not-an-array")

  and applyOp A.PlusOp   (IntV vli) (IntV vri) _ = IntV (vli + vri)
    | applyOp A.MinusOp  (IntV vli) (IntV vri) _ = IntV (vli - vri)
    | applyOp A.TimesOp  (IntV vli) (IntV vri) _ = IntV (vli * vri)
    | applyOp A.DivideOp (IntV vli) (IntV vri) _ = IntV (vli div vri)
    | applyOp A.EqOp  (IntV vli) (IntV vri) _ = BoolV (vli =  vri)
    | applyOp A.NeqOp (IntV vli) (IntV vri) _ = BoolV (vli <> vri)
    | applyOp A.LtOp  (IntV vli) (IntV vri) _ = BoolV (vli < vri)
    | applyOp A.LeOp  (IntV vli) (IntV vri) _ = BoolV (vli <= vri)
    | applyOp A.GtOp  (IntV vli) (IntV vri) _ = BoolV (vli > vri)
    | applyOp A.GeOp  (IntV vli) (IntV vri) _ = BoolV (vli >= vri)
    | applyOp A.EqOp   (StrV vls) (StrV vrs) _ = BoolV (vls = vrs)
    | applyOp A.NeqOp  (StrV vls) (StrV vrs) _ = BoolV (vls <> vrs)
    | applyOp _ _ _ pos = E.crashAt pos "inadmissible values at primitive application"
						      
  fun exec (outstream, e) = ( TextIO.output (outstream, "NOT IMPLEMENTED")
			    ; TextIO.flushOut outstream )

				(* base env w/ stdlib
				 * %BREAK
				 *)

end

(* Interpreter ... should work with an expty but MCIiML fuses type-checking and IR translation *)

structure Interp : 
     sig val exec : TextIO.outstream * Absyn.exp -> unit
     end =
struct

  structure A = Absyn
  structure E = ErrorMsg
  structure L = List
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
		 | FunV of S.symbol list * A.exp * env
		 | LibV of (value list -> A.pos -> value)
	         | UnitV

  withtype cell = value ref
			
       and env = S.symbol -> A.pos -> cell (* could use Table ... but its nice to be self-contained *)
				 
  fun show NilV = "nil"
    | show (IntV n) = Int.toString n
    | show (BoolV true)  =  "true"
    | show (BoolV false) =  "false"
    | show (StrV s) = s
    | show (ArrV vs) = "[" ^ (V.foldr (fn (v_, s) => (show (!v_)) ^ ", " ^ s) "" vs) ^ "]"
    | show (RecV ivs) = "{" ^ (L.foldr (fn ((i,v_),s) => S.name i ^ (show (!v_)) ^ ", " ^ s) "" ivs) ^ "}"
    | show (FunV _) = "A FUNCTION"
    | show (LibV _) = "A LIBRARY FUNCTION"
    | show UnitV = "unit"
	  
  fun emptyEnv i pos = E.crashAt pos ("variable `" ^ (S.name i) ^  "' not found.")
				      
  fun extEnv r i v = let val v_ = ref v
		      in fn i' => fn pos => if i=i' then v_
			                            else (r i' pos)
		     end

  fun extEnvL r [] = r
    | extEnvL r ((i,v)::ivs) = extEnv (extEnvL r ivs) i v
				       
  fun findEnv r i pos = (r i pos)
			  
  fun getEnv r i pos = !(findEnv r i pos)

  fun setEnv r i v pos = (findEnv r i pos) := v

  exception BREAK of A.pos
  exception EXIT of A.pos

  (* expressions -- evalExp : env -> exp -> value *)
  fun evalExp r (A.VarExp v) = !(evalVar r v)
    
    | evalExp _  A.NilExp            = NilV
    | evalExp _ (A.IntExp i)         = IntV i
    | evalExp _ (A.BoolExp b)        = BoolV b
    | evalExp _ (A.StringExp (s, _)) = StrV s
					    
    | evalExp r (A.CallExp{func,args,pos}) = (case (getEnv r func pos)
					       of (FunV (is, e, r')) => let val vs = map (evalExp r) args
								         in evalExp (extEnvL r'
											     (P.zipEq (is, vs)
											      handle  UnequalLengths => E.crashAt pos "wrong number of arguments to function"))
										    e
									    handle (BREAK p) => E.crashAt p ("break not inside loop")
									end
						| LibV f => f (map (evalExp r) args) pos
						| _ => E.crashAt pos ("calling " ^ (S.name func) ^ " : not-a-function"))

    | evalExp r (A.OpExp{left,oper,right,pos}) = let val vl = evalExp r left
						     val vr = evalExp r right
						  in applyOp oper vl vr pos
						 end
							    
    | evalExp r (A.RecordExp{fields, typ, pos}) = let fun evalField (i,e,p) = (i, ref (evalExp r e))
						      val ivs = map evalField fields
						   in RecV ivs
						  end
										
    | evalExp r (A.SeqExp [])      = ( E.warn "empty sequence"
				     ; UnitV )

    | evalExp r (A.SeqExp [(e,p)]) = ( E.warnAt p "singleton sequence"
		 		     ; evalExp r e )
				     
    | evalExp r (A.SeqExp eps)     = foldl (fn ((e,_),v) => evalExp r e) UnitV eps

    | evalExp r (A.AssignExp{var=v,exp,pos}) = ( (evalVar r v) := (evalExp r exp)
					       ; UnitV
					       )
						   
    | evalExp r (A.IfExp{test,then',else',pos}) = (case (evalExp r test)
						    of (BoolV true)  => evalExp r then'
						     | (BoolV false) => (case else'
									  of NONE => UnitV
									  |  SOME e => evalExp r e)
						     | _ => E.crashAt pos "if-test not boolean")

    | evalExp r (A.WhileExp{test,body,pos}) = let fun loop () = case (evalExp r test)
								 of (BoolV false) => UnitV
								  | (BoolV true) => ( evalExp r body
										    ; loop () )
								  | _ => E.crashAt pos "while-test not boolean"
					       in (loop ()
						   handle (BREAK _) => UnitV)
					      end
								      
    | evalExp r (A.ForExp{var,escape,lo,hi,body,pos}) = let val si = case evalExp r lo
								      of (IntV n) => n
								       | _ => E.crashAt pos "for-loop start not-integer"
							    val ei = case evalExp r hi
								      of (IntV n) => n
								       | _ => E.crashAt pos "for-loop end not-integer"
							    val tst = if (si < ei) then (fn i => i <= ei)
								                   else (fn i => i >= ei)
							    val r = extEnv r var (IntV si)
							    val v_ = findEnv r var pos
							    fun loop () = (case (!v_)
									    of (IntV n) => if (tst n) then ( evalExp r body
													   ; v_ := IntV (n+1)
													   ; loop () )
											              else UnitV
									     | _ => E.crashAt pos "for-loop index not-integer")
							 in loop ()
							    handle (BREAK _) => UnitV
							end

    | evalExp r (A.BreakExp pos) = raise (BREAK pos)

    | evalExp r (A.LetExp{decs,body,pos}) = let fun loop [] r = evalExp r body
						  | loop (d::ds) r = loop ds (evalDecl r d)
					     in loop decs r
					    end

    | evalExp r (A.ArrayExp{typ,size,init,pos}) = let val n = case (evalExp r size)
							       of (IntV n) => if (n<0) then E.crashAt pos "negative-size array"
									      else n
								| _ => E.crashAt pos "non-integer array size"
						      val v = evalExp r init
						  in 
						      ArrV (V.tabulate (n, (fn _ => ref v)))
						  end

  (* evalDecl computes a new environment, not a new value *)
  and evalDecl r (A.FunctionDec fds) = let fun pname {name,escape,typ,pos} = name
					   val r = extEnvL r (map (fn ({name,params,result,body,pos}) => (name, UnitV)) fds)
				        in ( L.app (fn {name,params,result,body,pos} => setEnv r
											       name
											       (FunV ((map (fn {name,escape,typ,pos}=>name) params), body, r))
											       pos)
						   fds
					   ; r )
				       end
    | evalDecl r (A.TypeDec _) = r

    | evalDecl r (A.VarDec {name,escape,typ,init,pos}) = let val v = evalExp r init
							 in extEnv r name v
							 end

  (* variables -- evalVar : env -> var -> ref value *)
  and evalVar r (A.SimpleVar(s, p)) = findEnv r s p
					      
    | evalVar r (A.FieldVar(v, s, p)) = (case !(evalVar r v)
					  of RecV svs => (case (L.find (fn (s',_) => s'=s) svs)
							   of NONE => E.crashAt p ("field " ^ (S.name s) ^ " not found")
							    | SOME (_,v_) => v_)
					   | _ => E.crashAt p ("field " ^ (S.name s) ^ " for not-a-record"))
    | evalVar r (A.SubscriptVar(v, e, p)) = case !(evalVar r v)
					     of ArrV vs => (case (evalExp r e)
							     of (IntV n) => if (n<0) orelse (n>=(V.length vs))
									    then E.crashAt p ("array index " ^ (Int.toString n) ^ " out-of-bounds")
									    else (V.sub (vs, n))
							      | _ => E.crashAt p "array index not integer")
					      | _ =>  E.crashAt p "index into not-an-array"

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

  fun printL outstream [StrV s] _   = ( TextIO.output (outstream, s)
				      ; UnitV
				      )
    | printL _         _        pos = E.crashAt pos "print: not-a-string"

  fun flushL outstream [] _   = ( TextIO.flushOut outstream
				; UnitV
				)
    | flushL _         _  pos = E.crashAt pos "flush: too-many-arguments"

  fun getCharL sn_ [] pos = (case !sn_
			     of NONE => StrV ""
			     |  (SOME (s, n)) => if (n >= String.size(s))
						 then ( case TextIO.inputLine TextIO.stdIn
							 of NONE => sn_ := NONE
							 |  SOME s => sn_ := SOME (s,0)
						      ; getCharL sn_ [] pos  )
						 else let val v = StrV(String.substring(s,n,1))
						      in ( sn_ := SOME (s, n+1)
							 ; v  )
						      end)
    | getCharL sn_ _  pos = E.crashAt pos "getchar: too-many-arguments"
      
  fun ordL [StrV s] _   = IntV (if s="" then 0-1
			                else Char.ord(String.sub(s,0)))
    | ordL _        pos = E.crashAt pos "ord: not-a-string"

  fun chrL [IntV n] _   = StrV (String.str (Char.chr n))
    | chrL _        pos = E.crashAt pos "chr: not-an-int"

  fun sizeL [StrV s] _   = IntV (String.size s)
    | sizeL _        pos = E.crashAt pos "size: not-a-string"

  fun substringL [StrV s, IntV f, IntV n] _   = let val l = String.size s
						 in if f<0 orelse f>l then (StrV "") (* first out-of-bounds *)
						    else if n<1 orelse f+n>=l then (StrV "") (* next out-of-bounds *)
						    else StrV (String.substring(s, f, n))
						end
    | substringL _                        pos = E.crashAt pos "substring: not-a-string or out-of-bounds"

  fun concatL [StrV s1, StrV s2] _   = StrV (s1 ^ s2)
    | concatL _                  pos = E.crashAt pos "concat: not two strings"

  fun notL [BoolV b] pos = ( E.warnAt pos "not called"
			   ; BoolV (not b)
			   )
    | notL _         pos = E.crashAt pos "not: not-a-boolean"

  fun exitL [IntV i] _ = raise (EXIT i)
    | exitL _      pos = E.crashAt pos "exit: not-an-int"
				      
  fun exec (outstream, e) = let val inline = ref (SOME ("", 1))
				val r = extEnvL emptyEnv
						(map (fn (s,f) => (S.symbol s, LibV f))
						     [ ("print"    , (printL outstream))
						     , ("flush"    , (flushL outstream))
						     , ("getchar"  , (getCharL inline))
						     , ("ord"      , ordL)
						     , ("chr"      , chrL)
						     , ("size"     , sizeL)
						     , ("substring", substringL)
						     , ("concat"   , concatL)
						     , ("not"      , notL)
						     , ("exit"     , exitL)
						     ])
			     in ( TextIO.output (outstream, "\n ended with " ^ (show (evalExp r e)) ^ "\n")
				  handle (BREAK pos) => E.crashAt pos "break out of tigerMain"
				       | (EXIT i)    => TextIO.output (outstream, "exited with " ^ (Int.toString i) ^ "\n")
				; TextIO.flushOut outstream )
			    end
end

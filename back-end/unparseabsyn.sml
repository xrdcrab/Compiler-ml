structure UnparseAbsyn : 
     sig val print : TextIO.outstream * Absyn.exp -> unit
         val opname : Absyn.oper -> string
         val commaSep : string list -> string
     end =
struct

  structure S = Symbol
  structure A = Absyn

  val linelen = 160

  fun >>= (f, g) = fn v => g (f v)
  infix >>=

  fun opname A.PlusOp = "+"
	 | opname A.MinusOp = "-"
	 | opname A.TimesOp = "*"
	 | opname A.DivideOp = "/"
	 | opname A.EqOp = "="
	 | opname A.NeqOp = "<>"
	 | opname A.LtOp = "<"
	 | opname A.LeOp = "<="
	 | opname A.GtOp = ">"
	 | opname A.GeOp = ">="

  fun print (outstream, e) =
      let fun say s = fn d => ( TextIO.output (outstream, s)
			      ; TextIO.flushOut outstream
			      ; d + size s )

	  val sayNothing = fn d => d

	  val sayLn = say "\n" >>= (fn _ => 0)

	  fun indent i = fn d => ((if i>d then say " " >>= indent i
				   else if i=d then sayNothing
				   else (sayLn >>= indent i)) d)

	  fun seplist i f s [] = sayNothing
	    | seplist i f s [a] = indent i >>= f a
	    | seplist i f s (h::t) = indent i >>= f h >>= s >>= seplist i f s t

	  fun termlist i f s [] = sayNothing
	    | termlist i f s (h::t) = indent i >>= f h >>= s >>= termlist i f s t
				  
	  fun var (A.SimpleVar(s,p)) = say (S.name s)
	    | var (A.FieldVar(v,s,p)) = var v >>= say "." >>= say (S.name s)
	    | var (A.SubscriptVar(v,e,p)) = var v >>= say "[" >>= exp e >>= say "]"
				  
	  and exp (A.VarExp v) = var v
	    | exp A.NilExp = say "nil"
	    | exp (A.IntExp i) = say (Int.toString i)
	    | exp (A.BoolExp b) = say (Bool.toString b)
	    | exp (A.StringExp(s,p)) = say "\"" >>= say (String.toString s) >>= say "\""

	    | exp (A.CallExp{func,args,pos}) = say (S.name func)
					       >>=  say "("
					       >>=  (fn i => seplist i exp (say ",") args i)
					       >>= say ")"

	    | exp (A.OpExp{left,oper,right,pos}) = say "("
						   >>= (fn i => (exp left
							       >>= (fn d => let val rest = say (opname oper) >>= exp right
									  in (if d>linelen then indent i >>= rest
									     else rest) d
									  end)) i)
						   >>= say ")"

	    | exp(A.RecordExp{fields,typ,pos}) = let fun f (name,e,_) = say (S.name name) >>= say " = " >>= exp e
						 in say (S.name typ)
						    >>= say "{"
						    >>= (fn i => seplist i f (say ",") fields i)
						    >>= say "}"
						 end

	    | exp (A.SeqExp l)  = say "("
				  >>= (fn i => seplist i (fn (e,p) => exp e) (say ";") l i)
				  >>= say ")"

	    | exp (A.AssignExp{var=v,exp=e,pos}) = var v >>= say " := " >>= exp e

	    | exp (A.IfExp{test,then',else',pos}) = (fn i => (say " if " >>= exp test
							      >>= indent i >>= say " then " >>= exp then'
							      >>= indent i >>= (case else' of
										NONE => sayNothing
									      | SOME e => say " else " >>= exp e)) i)

	    | exp (A.WhileExp{test,body,pos}) = (fn i => (say " while " >>= exp test
							  >>= indent i >>= say " do " >>= exp body) i)

	    | exp(A.ForExp{var=v,escape=b,lo,hi,body,pos}) = (fn i => (say " for " >>= say (S.name v)
								       >>= say " := " >>= exp lo
								       >>= say " to " >>= exp hi
								       >>= indent i >>= say " do " >>= exp body) i)

	    | exp (A.BreakExp pos) = say " break "

	    | exp (A.LetExp{decs,body,pos}) = (fn i' => (say " let "
							 >>= (fn i => seplist i dec sayNothing decs i)
							 >>= indent i' >>= say "  in " >>= exp body
							 >>= indent i' >>= say " end ") i')

	    | exp (A.ArrayExp{typ,size,init,pos}) = say (S.name typ) >>= say "[" >>= exp size >>= say "] of " >>= exp init

	  and dec (A.FunctionDec decs) = let fun field {name,escape,typ,pos} = say (S.name name) 
									       >>= say " : "
									       >>= say (S.name typ)
					     and fdec {name,params,result,body,pos} = say "function "
										      >>= say (S.name name)
										      >>= say "("
										      >>= (fn i => seplist i field (say ",") params i)
										      >>= say ") "
										      >>= (case result of
											     NONE => sayNothing
											   | SOME (typ,pos) => say ": " >>= say (S.name typ))
										      >>= say " = " >>= exp body
										      >>= sayLn
					 in (fn i => seplist i fdec sayNothing decs i)
					 end

	    | dec (A.VarDec{name,escape,typ,init,pos}) = say "var " >>= say (S.name name) 
							     >>= (case typ of NONE => sayNothing
									    | SOME (tname, _) => say ":" >>= say (S.name tname))
							     >>= say " := " >>= exp init

	    | dec (A.TypeDec decs) = let fun tdec {name,ty=t,pos} = say "type " >>= say (S.name name) >>= say " = " >>= ty t >>= sayLn
				     in (fn i => seplist i tdec sayNothing decs i)
				     end

	  and ty (A.NameTy (s,p)) = say (S.name s)
	    | ty (A.RecordTy l) = let fun f {name,escape,typ,pos}  = say (S.name name) >>= say " : " >>= say (S.name typ)
				  in  say " { "
				      >>= (fn i => seplist i f (say ",") l i)
				      >>= say "}"
				  end

	    | ty (A.ArrayTy (s,p)) = say "array of " >>= say (S.name s)

      in  ( exp e 0
	  ; TextIO.output (outstream, "\n")
	  ; TextIO.flushOut outstream )
      end

  fun commaSep [] = ""
    | commaSep [i] = i
    | commaSep (h::t) = h ^ "," ^ (commaSep t)

end


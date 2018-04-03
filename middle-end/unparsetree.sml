structure UnparseTree :
     sig val print : TextIO.outstream * Tree.stm -> unit end =
struct

  structure R = Tree
  structure S = Symbol

  val labelWidth = 6

  fun >>= (f, g) = fn v => g (f v)
  infix >>=

  fun print (outstream, s) =

      let fun say s = fn d => ( TextIO.output (outstream, s)
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
      
	  fun binop R.PLUS = say " PLUS "
	    | binop R.MINUS = say " MINUS "
	    | binop R.MUL = say " MUL "
	    | binop R.DIV = say " DIV "
	    | binop R.AND = say " AND "
	    | binop R.OR = say " OR "
	    | binop R.LSHIFT = say " LSHIFT "
	    | binop R.RSHIFT = say " RSHIFT "
	    | binop R.ARSHIFT = say " ARSHIFT "
	    | binop R.XOR = say " XOR "

	  fun relop R.EQ = say " EQ "
	    | relop R.NE = say " NE "
	    | relop R.LT = say " LT "
	    | relop R.GT = say " GT "
	    | relop R.LE = say " LE "
	    | relop R.GE = say " GE "
	    | relop R.ULT = say " ULT "
	    | relop R.ULE = say " ULE "
	    | relop R.UGT = say " UGT "
	    | relop R.UGE = say " UGE "

(* ESEQ is written as { s ; e }
 * CALL is written as CALL e ( e, e, e )
 * TEMP is written as %t
 * MEM  is written as [...]
 * LABEL is written as L:
 *)
	  fun stm (R.SEQ (s, s'), i) = stm (s, i) >>= stm (s', i)
            | stm (R.LABEL l, i) = indent (i-labelWidth) >>= say (S.name l) >>= say ":"
            | stm (R.JUMP (e, ls), i) = indent i >>= say "JUMP " >>= exp e
            | stm (R.CJUMP (r, e, e', l, l'), i) = indent i
						   >>= say "CJUMP "
						   >>= exp e
						   >>= relop r
						   >>= exp e'
						   >>= say " ? "
						   >>= say (S.name l)
						   >>= say " : "
						   >>= say (S.name l')
	    | stm (R.MOVE (e, e'), i) = indent i >>= say "MOVE " >>= (fn i => (exp e >>= indent i >>= exp e') i)
            | stm (R.EXP e, i) = indent i >>= exp e

	  and exp (R.BINOP (b, e, e')) = say "<" >>= exp e >>= binop b >>= exp e' >>= say ">"
	    | exp (R.MEM e) = say "[" >>= exp e >>= say "]"
	    | exp (R.TEMP t) = say "%" >>= say (Int.toString t)
	    | exp (R.ESEQ (s, e)) = (fn i => (say "{ " >>= stm (s,i+labelWidth) >>= indent i >>= say "; " >>= exp e >>= say " }") i)
	    | exp (R.NAME l) = say (S.name l)
	    | exp (R.CONST i) = say (Int.toString i)
	    | exp (R.CALL (e, es)) = say "CALL " >>= exp e >>= say " (" >>= (fn i => seplist i exp (say ",") es i) >>= say ")"

	      
      in ( stm (s, labelWidth+2) 0
	 ; TextIO.output (outstream, "\n")
	 ; TextIO.flushOut outstream )
      end
end

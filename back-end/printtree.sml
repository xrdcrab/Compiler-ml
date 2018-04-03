structure PrintTree : 
     sig val print : TextIO.outstream * Tree.stm -> unit end =
struct

structure T = Tree
  fun print (outstream, s) =
      let fun say s =  TextIO.output(outstream,s)

          fun sayLn s = say (s ^ "\n")

          fun indent 0 = ()
            | indent i = ( say " "
                         ; indent(i-1)
                         )

          fun prStm(T.SEQ(a,b), d) = ( indent d; sayLn "SEQ("
                                     ;   prStm(a,d+1); sayLn ","
                                     ;   prStm(b,d+1)
                                     ; say ")"
                                     )
                                     
            | prStm(T.LABEL lab, d) = ( indent d; say ("LABEL " ^ (Symbol.name lab)) )
                                      
            | prStm(T.JUMP (e,_), d) = ( indent d; sayLn "JUMP("
                                       ;   prExp(e,d+1)
                                       ; say ")"
                                       )
                                       
            | prStm(T.CJUMP(r,a,b,t,f), d) = ( indent d; sayLn ("CJUMP(" ^ (relop r) ^ ",")
                                             ;   prExp(a,d+1); sayLn ","
                                             ;   prExp(b,d+1); sayLn ","
                                             ;   indent(d+1); say ((Symbol.name t) ^ ",")
                                             ;   say (Symbol.name f)
                                             ; say ")")
                                             
            | prStm(T.MOVE(a,b), d) = ( indent d; sayLn "MOVE("
                                      ;   prExp(a,d+1); sayLn ","
			                             ;   prExp(b,d+1)
                                      ; say ")"
                                      )
                                      
            | prStm(T.EXP e, d) = ( indent d; sayLn "EXP("
                                  ;   prExp(e,d+1)
                                  ; say ")"
                                  )
                                  
          and prExp(T.BINOP(p,a,b), d) = ( indent d; sayLn ("BINOP(" ^ (binop p) ^ ",")
			                                ;   prExp(a,d+1); sayLn ","
                                         ;   prExp(b,d+1)
                                         ; say ")"
                                         )

            | prExp(T.MEM(e), d) = ( indent d; sayLn "MEM("
                                   ;   prExp(e,d+1)
                                   ; say ")"
                                   )

            | prExp(T.TEMP t, d) = ( indent d; say ("TEMP t" ^ (Int.toString t)) )

            | prExp(T.ESEQ(s,e), d) = ( indent d; sayLn "ESEQ("
                                      ;   prStm(s,d+1); sayLn ","
			                             ;   prExp(e,d+1)
                                      ; say ")"
                                      )

            | prExp(T.NAME lab, d) = ( indent d; say ("NAME " ^ (Symbol.name lab)) )

            | prExp(T.CONST i, d) = ( indent d; say ("CONST " ^ (Int.toString i)) )

            | prExp(T.CALL(e,el), d) = ( indent d; sayLn "CALL("
                                       ;   prExp(e,d+1)
			                              ;   app (fn a => (sayLn ","; prExp(a,d+2)))
                                               el
			                              ; say ")"
                                       )

    and binop T.PLUS    = "PLUS"
      | binop T.MINUS   = "MINUS"
      | binop T.MUL     = "MUL"
      | binop T.DIV     = "DIV"
      | binop T.AND     = "AND"
      | binop T.OR      = "OR"
      | binop T.LSHIFT  = "LSHIFT"
      | binop T.RSHIFT  = "RSHIFT"
      | binop T.ARSHIFT = "ARSHIFT"
      | binop T.XOR     = "XOR"

    and relop T.EQ  = "EQ"
      | relop T.NE  = "NE"
      | relop T.LT  = "LT"
      | relop T.GT  = "GT"
      | relop T.LE  = "LE"
      | relop T.GE  = "GE"
      | relop T.ULT = "ULT"
      | relop T.ULE = "ULE"
      | relop T.UGT = "UGT"
      | relop T.UGE = "UGE"
                      
   in prStm(s,0)
    ; sayLn ""
    ; TextIO.flushOut outstream
  end

end


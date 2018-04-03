signature TREE = 
sig 
  type label = Temp.label
  type temp = Temp.temp
  type size

  datatype stm = SEQ of stm * stm
               | LABEL of label
               | JUMP of exp * label list
               | CJUMP of relop * exp * exp * label * label
	            | MOVE of exp * exp
               | EXP of exp

       and exp = BINOP of binop * exp * exp
               | MEM of exp
               | TEMP of temp
               | ESEQ of stm * exp
               | NAME of label
               | CONST of int
	            | CALL of exp * exp list

       and binop = PLUS | MINUS
                 | MUL | DIV 
                 | AND | OR | XOR
                 | LSHIFT | RSHIFT | ARSHIFT
                                     
       and relop = EQ | NE
                 | LT | GT | LE | GE 
	              | ULT | ULE | UGT | UGE

  val notRel : relop -> relop
  val commute: relop -> relop
end

structure Tree : TREE = 
struct
  structure M = Temp

  type label = M.label
  type temp  = M.temp
  type size = int

  datatype stm = SEQ of stm * stm
               | LABEL of label
               | JUMP of exp * label list
               | CJUMP of relop * exp * exp * label * label
	            | MOVE of exp * exp
               | EXP of exp
			
       and exp = BINOP of binop * exp * exp
               | MEM of exp
               | TEMP of temp
               | ESEQ of stm * exp
               | NAME of label
               | CONST of int
	            | CALL of exp * exp list
			 
       and binop = PLUS | MINUS 
                 | MUL | DIV 
                 | AND | OR | XOR
                 | LSHIFT | RSHIFT | ARSHIFT
							  
       and relop = EQ | NE
                 | LT | GT | LE | GE 
	              | ULT | ULE | UGT | UGE
                                
  fun notRel _ = NE             (* fill this in *)
                   
  fun commute _ = ULE           (* fill this in *)
end

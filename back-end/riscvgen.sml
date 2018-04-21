structure RiscVGen : CODEGEN =
struct
    structure Frame = RiscVFrame
    structure A = Assem
    structure E = ErrorMsg
    structure F = RiscVFrame
    structure M = Temp
    structure R = Tree
    structure S = Symbol
    structure T = Temp
		      
    fun codegen frame stm =
	     let val ilist = ref []
                        
               (* ~i -> -i *)
               (*
	               	val int = fn : int -> string
					- val x = ~5;
					val x = ~5 : int
					- int (~x);
					val it = "5" : string
               *)
	        fun int i = if i >=0 then Int.toString(i) else "-" ^ Int.toString (abs i)

	        fun emit x = ilist := x :: !ilist
                         
	        fun result gen = let val t = T.newtemp()
			                    in ( gen t
				                    ; t )
			                    end

			(*list as destinations of the CALL, 
			so later phases of compier know what 
			something happens to them here*)
            val calldefs = ()

            (* in CJUMP? *)
            fun munchRelOp R.EQ = "beq"
            	|munchRelOp R.NE = "bne"
            	|munchRelOp R.LT = "blt"
            	|munchRelOp R.GE = "bge"
            	|munchRelOp R.ULT = "bltu"
            	|munchRelOp R.UGE = "bgeu"
            	|munchRelOp R.GT = "blt"
            	|munchRelOp R.LE = "bge"
            	|munchRelOp R.UGT = "bltu"
            	|munchRelOp R.ULE = "bgeu"

     		and munchBinOp R.PLUS = "add"
            	|munchBinOp R.MINUS = "sub"
            	|munchBinOp R.MUL = "mul"
            	|munchBinOp R.DIV = "div"
            	|munchBinOp R.AND = "and"
            	|munchBinOp R.OR = "or"
            	|munchBinOp R.XOR = "xor"
            	|munchBinOp R.LSHIFT = "sll"
            	|munchBinOp R.RSHIFT = "srl"
            	|munchBinOp R.ARSHIFT = "sra"

            and munchBinOpImm R.PLUS = "addi"
            	|munchBinOpImm R.AND = "andi"
            	|munchBinOpImm R.OR = "ori"
            	|munchBinOpImm R.XOR = "xori"
            	|munchBinOpImm R.LSHIFT = "slli"
            	|munchBinOpImm R.RSHIFT = "srli"
            	|munchBinOpImm R.ARSHIFT = "srai"
            	|munchBinOpImm R.MINUS = "addi"
            	|munchBinOpImm _ = "mul,div imm line 69"
			
				(* Tree.stm -> unit*)
				(* SEQ stm * stm *)
	        and munchStm(R.SEQ(a, b)) = (munchStm a; munchStm b)

         		(* MOVE exp * exp *)
         		(* 	addi  $t0, $zero, -1       # = -1
					sw    $t0, 28($gp)         # A[0] = -1
				*)
         		|munchStm(R.MOVE(R.MEM(R.BINOP(R.PLUS, e1, R.CONST i)), e2)) =
         			emit(A.OPER{
         				assem="sw `s1, " ^ int i ^ "(`s0) \n",
         					src=[munchExp e1, munchExp e2],
         					dst=[], jump=NONE
         				})

         		|munchStm(R.MOVE(R.MEM(R.BINOP(R.PLUS, R.CONST i, e1)), e2)) =
         			emit(A.OPER{
         				assem="sw `s1, " ^ int i ^ "(`s0) \n",
         					src=[munchExp e1, munchExp e2],
         					dst=[], jump=NONE
     					})

         		|munchStm(R.MOVE(R.MEM(R.BINOP(R.MINUS, e1, R.CONST i)), e2)) =
         			emit(A.OPER{
         				assem="sw `s1, " ^ int (~i) ^ "(`s0) \n",
         					src=[munchExp e1, munchExp e2],
         					dst=[], jump=NONE
         				})

         		|munchStm(R.MOVE(R.MEM(R.BINOP(R.MINUS, R.CONST i, e1)), e2))=
         			emit(A.OPER{
         				assem="sw `s1, " ^ int (~i) ^ "(`s0) \n",
         					src=[munchExp e1, munchExp e2],
         					dst=[], jump=NONE
     					})

         		|munchStm(R.MOVE(R.MEM(e1), R.MEM(e2))) =
         			emit(A.OPER{
         				assem="addi `s0, `s1, " ^int 0 ^ " \n",      (* "mv `s0 `s1 \n" *)
         					src=[munchExp e1, munchExp e2],
         					dst=[], jump=NONE
         				})

         		|munchStm(R.MOVE(R.MEM(R.CONST i), e2)) =
         			emit(A.OPER{
         				assem="sw `s0, " ^ int i ^ "($zero) \n ",
         					src=[munchExp e2], 
         					dst=[], jump=NONE
         				})                                           (* ?? is it correct?? *)

         		|munchStm(R.MOVE(R.MEM(e1), e2)) =
         			emit(A.OPER{
         				assem="sw `s1, 0(`s0) \n",
         					src=[munchExp e1, munchExp e2],
         					dst=[], jump=NONE
         				})

         		|munchStm(R.MOVE(R.TEMP t, e2)) =
         			emit(A.OPER{
         				assem="add `d0, `s0, `($zero) \n",           (* "mv `d0, `s0 \n" *)
         					src=[munchExp e2],
         					dst=[t], jump=NONE
         				})
         		
         		(* LABEL label*)
         		|munchStm(R.LABEL lab) = 
         		    emit(A.LABEL{
         		    	assem = S.name lab ^ ": \n", lab=lab
         		    })

         		(* call *) 
         		|munchStm(R.EXP(R.CALL(R.NAME(label), args))) = 
         			emit(A.OPER
         				{assem="jal `j0 \n",
         					src=munchArgs(0,args),
         					dst=F.RAreg::F.rvregs::F.tregs,
         					jump=SOME([label])
         				})                                            (* is it correct?? *)
         		
         		(* JUMP exp * label list *)
         		|munchStm(R.JUMP(R.NAME(jumpToLabel), labels)) =
         			emit(A.OPER{
         				assem="j `j0 \n",                             (* "j "^ S.name jumpToLabel ^ "\n" *)
         					src=[], dst=[], jump=SOME([jumpToLabel])
         				})
         		
         		|munchStm(R.JUMP(e, labels)) =
         			emit(A.OPER{
         				assem="jr `s0 \n",
         					src=[munchExp e], dst=[], jump=SOME(labels)
         				})

         		(* CJUMP relop * exp * exp * label * label *)
         		(* beq s0, s1, j0        if true, jumpTo l1, else next instruction
         		 jumpTo j1...            		  jumpTo l2
         		 l1...
         		 l2... *)
         		(* does not has instructions*)
         		(*  BGT, BGTU, BLE, and BLEU can be synthesized by reversing the 
         					operands to BLT, BLTU, BGE, and BGEU, respectively.*)
         		|munchStm(R.CJUMP(R.GT, e1, e2, l1, l2)) = 
         			emit(A.OPER{
         				assem=munchRelOp R.LT ^ "`s1, `s0, `j0 \n j `j1 \n",
         					src=[munchExp e1, munchExp e2], dst=[], jump=SOME([l1,l2])
         				})

         		|munchStm(R.CJUMP(R.LE, e1, e2, l1, l2)) = 
         			emit(A.OPER{
         				assem=munchRelOp R.GE ^ "`s1, `s0, `j0 \n j `j1 \n",
         					src=[munchExp e1, munchExp e2], dst=[], jump=SOME([l1,l2])
         				})

         		|munchStm(R.CJUMP(R.UGT, e1, e2, l1, l2)) = 
         			emit(A.OPER{
         				assem=munchRelOp R.ULT ^ "`s1, `s0, `j0 \n j `j1 \n",
         					src=[munchExp e1, munchExp e2], dst=[], jump=SOME([l1,l2])
         				})

         		|munchStm(R.CJUMP(R.ULE, e1, e2, l1, l2)) = 
         			emit(A.OPER{
         				assem=munchRelOp R.UGE ^ "`s1, `s0, `j0 \n j `j1 \n",
         					src=[munchExp e1, munchExp e2], dst=[], jump=SOME([l1,l2])
         				})

         		(* has instructions *)
         		|munchStm(R.CJUMP(relop, e1, e2, l1, l2)) = 
         			emit(A.OPER{
         				assem=munchRelOp relop ^ "`s0, `s1, `j0 \n j `j1 \n",
         					src=[munchExp e1, munchExp e2], dst=[], jump=SOME([l1,l2])
         				})

         		(* EXP *)
         		|munchStm(R.EXP e) = (munchExp e; ())

       			|munchStm _ = E.impossible "codegen stm 203"

						(* mem constant *)
						(* load and store in mem *)
				(* Tree.exp -> Tree.temp *)
	        and munchExp(R.MEM(R.CONST i)) =
					result(fn r => emit(A.OPER
							{assem = "lw `d0, "^int i^" (`d0) \n",
								src=[], dst=[r], jump=NONE
								}))

   						(* mem binop *)
						(* mem exp + const i *)	
				|munchExp (R.MEM(R.BINOP(R.PLUS, e1, R.CONST i))) = 
	          			result(fn r => emit(A.OPER
	          				{assem = "lw `d0, " ^int i^ "(`s0) \n",
	          					src = [munchExp e1], dst = [r], jump = NONE
	          					}))

	          		(* mem i plus e1 *)
		      	|munchExp(R.MEM(R.BINOP(R.PLUS, R.CONST i, e1))) =
	          		result(fn r => emit(A.OPER
	              			{assem = "lw `d0, " ^int i^ "(`s0) \n",
	              				src=[munchExp e1], dst=[r], jump=NONE
	              				}))

		    	|munchExp(R.MEM(R.BINOP(R.MINUS, e1, R.CONST i))) = 
	          			result(fn r => emit(A.OPER
	          				{assem = "lw `d0, " ^int (~i)^ "(`s0) \n",
	          					src = [munchExp e1], dst = [r], jump = NONE
	          					}))

			    |munchExp(R.MEM(R.BINOP(R.MINUS, R.CONST i, e1))) =
	          		result(fn r => emit(A.OPER
	              			{assem = "lw `d0, " ^int (~i)^ "(`s0) \n",
	              				src=[munchExp e1], dst=[r], jump=NONE
	              				}))
						
	 		   		(* mem exp *)
			  	|munchExp(R.MEM(e1)) =
					result(fn r => emit(A.OPER
							{assem = "lw `d0, 0(`s0) \n",
								src=[munchExp e1], dst=[r], jump=NONE
								}))

                 (* exp binop imm *) 
        		|munchExp(R.BINOP(binop, e1, R.CONST i)) = if binop = R.MINUS 
        		then
        			(* e1 - i  = e1 + (-i) *)
					result(fn r => emit(A.OPER
							{assem = "addi `d0, `s0," ^ int (~i) ^ "\n",
								src=[munchExp e1], dst=[r], jump=NONE
							}))
				else
					result(fn r => emit(A.OPER
							{assem = munchBinOpImm binop ^ " `d0, `s0," ^ int i ^ "\n",
								src=[munchExp e1], dst=[r], jump=NONE
							})) 					

				(* imm binop exp *)
			  	|munchExp(R.BINOP(R.PLUS, R.CONST i, e1)) =
					result(fn r => emit(A.OPER
							{assem = munchBinOpImm R.PLUS ^ " `d0, `s0, " ^ int i ^ "\n",
								src=[munchExp e1], dst=[r], jump=NONE
							}))

			  	|munchExp(R.BINOP(R.AND, R.CONST i, e1)) = 
					result(fn r => emit(A.OPER
							{assem = munchBinOpImm R.AND ^ " `d0, `s0, " ^ int i ^ "\n",
								src=[munchExp e1], dst=[r], jump=NONE
								}))

			  	|munchExp(R.BINOP(R.OR, R.CONST i, e1)) = 
					result(fn r => emit(A.OPER
							{assem = munchBinOpImm R.OR ^ " `d0, `s0, " ^ int i ^ "\n",
								src=[munchExp e1], dst=[r], jump=NONE
								}))

				|munchExp(R.BINOP(R.LSHIFT, R.CONST i, e1)) = 
					result(fn r => emit(A.OPER
							{assem = munchBinOpImm R.LSHIFT ^ " `d0, `s0, " ^ int i ^ "\n",
								src=[munchExp e1], dst=[r], jump=NONE
								}))

			  	|munchExp(R.BINOP(R.RSHIFT, R.CONST i, e1)) =  
					result(fn r => emit(A.OPER
							{assem = munchBinOpImm R.RSHIFT ^ " `d0, `s0, " ^ int i ^ "\n",
								src=[munchExp e1], dst=[r], jump=NONE
								}))

			  	|munchExp(R.BINOP(R.XOR, R.CONST i, e1)) = 
					result(fn r => emit(A.OPER
							{assem = munchBinOpImm R.XOR ^ " `d0, `s0, " ^ int i ^ "\n",
								src=[munchExp e1], dst=[r], jump=NONE
								}))

		  		|munchExp(R.BINOP(R.ARSHIFT, R.CONST i, e1)) = 
					result(fn r => emit(A.OPER
							{assem = munchBinOpImm R.ARSHIFT ^ " `d0, `s0, " ^ int i ^ "\n",
								src=[munchExp e1], dst=[r], jump=NONE
								}))

				(* exp binop exp *)
			  	|munchExp(R.BINOP(binop, e1, e2)) =
					result(fn r => emit(A.OPER
							{assem = munchBinOp binop ^ " `d0, `s0, `s1 \n",
								src=[munchExp e1, munchExp e2], dst=[r], jump=NONE
							}))	

	          		(* name *)
			  	|munchExp(R.NAME label) =
					result(fn r => emit(A.LABEL
						{assem = S.name label ^ ": \n", lab=label
					}))
							(*{assem = "la `d0, "^ S.name label ^ "\n",
															src=[], dst=[r], jump=NONE
															}))*)

    				(* constant expression *)  
			  	|munchExp (R.CONST i) =
					result (fn r => emit (A.OPER 
						{assem = "li `d0, " ^int i^"\n",
							src=[], dst=[r], jump=NONE
							}))

					(* temp *)
			  	|munchExp(R.TEMP t) = t

			  	(* ESEQS will be removed by canonicalization *)

					(* call *) (*
			  	|munchExp(R.CALL(e, args)) =
					result(fn r => emit(A.OPER
							{assem = "CALL `s0 \n",
								src=[munchExp(e) :: munchArgs(0,args)], dst=calldefs, jump=NONE
								})) *)

	          	|munchExp _ = E.impossible "CODEGEN EXP MISMATCH"
                                          
            (* move all arguments to correct positions*)                   
	        and munchArgs (i, es) = 
		         	let 
		         		fun munchArg (e, F.InFrame n, (i, args)) = 
								let 
									val src = munchExp e
			                    in ( emit (A.OPER { assem = "sw `s0, " ^ int n ^ "(`fp) \n"    (* is $ needed? *)
			                                             , src   = [src]
			                                             , dst   = []
			                                             , jump  = NONE
			                                 	}
			                         	)
			                            	; (i+1, src::args)
			                  	)
		                    	end
		               		|munchArg (e, F.InReg t, (i, args)) = 
		               			let 
		               				val src = munchExp e
			                    in ( emit (A.MOVE { assem="mv `d0 `s0 \n",
			                                                 src=[src],
			                                                 dst=[t]
			                                   }
			                           )
			                              ; (i+1, src::args)
			                    )
					            end

			            val {name,formals,sp} = frame
			        in 
			        	let 
			        		val (_, args) = ListPair.foldl munchArg (i, []) (es, formals)
			            in args
			            end
			        end
                                    
	     in ( munchStm stm
	        ; rev (!ilist)
           )
	     end
end

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
                        
	         val int = Int.toString
                      
	         fun emit x = ilist := x :: !ilist
                         
	         fun result gen = let val t = T.newtemp()
			                    in ( gen t
				                    ; t )
			                    end
                             
            fun munchRelOp _ = "a relop"

	         and  munchStm (R.MOVE (e1, e2)) = emit (A.OPER { assem = "some instruction\n"
                                                           , src   = []
								                                   , dst   = []
                                                           , jump  = NONE
                                                           }
                                                   )
               (* ... add cases *)
	            | munchStm _ = E.impossible "CODEGEN STM MISMATCH"
                              
            and munchBinOp _ = "a binop"
                               
	         and munchExp (R.CONST i) = result (fn r => emit (A.OPER { assem = "some instruction\n"
                                                                    , src   = []
								                                            , dst   = []
								                                            , jump  = NONE
                                                                    }
                                              ))
              (* ... add cases *)
              | munchExp _ = E.impossible "CODEGEN EXP MISMATCH"
                                           
	         and munchArgs (i, es) = let fun munchArg (e, F.InFrame n, (i, args)) = let val src = munchExp e
										                                                     in ( emit (A.OPER { assem = "some instruction\n"
												                                                                 , src   = []
												                                                                 , dst   = []
												                                                                 , jump  = NONE
                                                                                                     }
                                                                                             )
										                                                        ; (i+1, src::args)
                                                                                      )
										                                                     end
					                           | munchArg (e, F.InReg t, (i, args)) = let val src = munchExp e
										                                                   in ( emit (A.MOVE { assem="some instruction\n",
												                                                                 src=src,
												                                                                 dst=t
                                                                                                   }
                                                                                           )
										                                                      ; (i+1, src::args)
                                                                                    )
										                                                   end
					                         val {name,formals,sp} = frame
				                        in let val (_, args) = ListPair.foldl munchArg (i, []) (es, formals)
				                           in args
				                           end
				                        end
                                    
	     in ( munchStm stm
	        ; rev (!ilist)
           )
	     end
end

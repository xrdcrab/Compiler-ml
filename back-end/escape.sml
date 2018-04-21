structure FindEscape : sig
    val findEscape : Absyn.exp -> unit 
end =
struct
   structure A = Absyn
   structure E = ErrorMsg
   structure S = Symbol

   type depth = int

   type escEnv = (depth * bool ref) S.table

   fun addVar (env, name, d, escape) = ( escape := false
				                           ; S.enter (env, name, (d, escape))
                                       )
   fun escapeExp (env, d) =
       let fun escaped name = case S.look (env, name)
			                      of NONE => ()
				                    | SOME (d', r) => if d'<d
						                                then r := true
						                                else ()
                                                       
	        and escVar (A.SimpleVar (name, pos)) = escaped name
	          | escVar (A.FieldVar (var, name, pos)) = escVar var
	          | escVar (A.SubscriptVar (var, exp, pos)) = ( escVar var
							                                    ; escExp exp
                                                         )

	        and escExp (A.VarExp var) = escVar var

	          | escExp (A.NilExp) = ()
                                   
	          | escExp (A.IntExp _) = ()

             | escExp (A.BoolExp _) = ()
                                     
	          | escExp (A.StringExp _) = ()
                                         
	          | escExp (A.CallExp{func, args, pos}) = app escExp args
						                                 
	          | escExp (A.OpExp{left, oper, right, pos}) = ( escExp left
						                                        ; escExp right
                                                          )

	          | escExp (A.ArrayExp{typ, size, init, pos}) = ( escExp size
						                                         ; escExp init
                                                           )
                                                        
	          | escExp (A.RecordExp{fields, typ, pos}) = app (fn (_, exp, _) => escExp exp)
							                                       fields
                                                          
	          | escExp (A.SeqExp exps) = app (fn (exp, _) => escExp exp)
					                             exps
                                            
	          | escExp (A.AssignExp{var, exp, pos}) = ( escExp exp
						                                   ; escVar var
                                                     )
						  
	          | escExp (A.IfExp{test, then', else'=NONE, pos}) = ( escExp test
							                                           ; escExp then'
                                                                )

	          | escExp (A.IfExp{test, then', else'=SOME else', pos}) = ( escExp test
								                                              ; escExp then'
								                                              ; escExp else' )

	          | escExp (A.WhileExp{test, body, pos}) = ( escExp test
						                                    ; escExp body
                                                      )

	          | escExp (A.BreakExp _) = ()

	          | escExp (A.LetExp{decs, body, pos}) = let val env = foldl escDec env decs
						                                  in escapeExp (env, d) body
						                                  end

             | escExp (A.ForExp _) = E.impossible "FOR NOT TRANSLATED"
						 
	        and escDec (A.FunctionDec nfs, env) =
               let fun addArg ({name, escape, typ, pos}, env) = addVar (env, name, d+1, escape)
                   fun escFun {name, params, result, body, pos} = let val fenv = foldl addArg env params
											                                 in escapeExp (fenv, d+1) body
											                                 end
					in ( app escFun nfs
						; env
                  )
					end
					
	          | escDec (A.VarDec{name, escape, typ, init, pos}, env) = ( escExp init
								                                              ; addVar (env, name, d, escape)
                                                                      )
								                                          
	          | escDec (A.TypeDec nts, env) = env
       in escExp
       end

   val findEscape : (A.exp -> unit) = escapeExp (S.empty, 0)
end

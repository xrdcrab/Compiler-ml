structure Semant : SEMANT =
struct
  structure A = Absyn
  structure E = ErrorMsg
  structure M = Temp
  structure S = Symbol
  structure T = Types
  structure U = UnparseAbsyn
  structure V = Env
  structure X = Translate
		
  type expty = { exp : X.exp
	            , ty : T.ty
               }

  datatype operType = ARITHMETIC | EQUALITY | COMPARISON

  fun opType   A.PlusOp = ARITHMETIC
	 | opType  A.MinusOp = ARITHMETIC
	 | opType  A.TimesOp = ARITHMETIC
	 | opType A.DivideOp = ARITHMETIC
	 | opType     A.EqOp = EQUALITY
	 | opType    A.NeqOp = EQUALITY
	 | opType     A.LtOp = COMPARISON
	 | opType     A.LeOp = COMPARISON
	 | opType     A.GtOp = COMPARISON
	 | opType     A.GeOp = COMPARISON


  fun typeError pos msg = E.error pos ("type error: " ^ msg)
			  
  fun commaSep [] = ""
    | commaSep [s] = s
    | commaSep (s::ss) = s ^ ", " ^ (commaSep ss)
			 
  fun unparseTy T.INT    = "int"
    | unparseTy T.BOOL   = "bool"
    | unparseTy T.STRING = "string"
    | unparseTy T.UNIT   = "unit"
    | unparseTy T.NIL    = "nil"
    | unparseTy (T.NAME (name, ref NONE)) = "unknown type " ^ (S.name name)
    | unparseTy (T.NAME (name, ref (SOME _))) = S.name name
    | unparseTy (T.ARRAY (ty, _)) = "[" ^ (unparseTy ty) ^ "]"
    | unparseTy (T.RECORD (sts, _)) = "{"
				                          ^ (commaSep (map (fn (name, ty) => (S.name name)
									                                              ^ ":"
									                                              ^ (unparseTy ty)) sts))
				                          ^ "}"
				      
  fun checkDups (s, []) = ()
    | checkDups (s, (n,p)::nps) = ( case List.find (fn (n',_) => n=n') nps
				                         of NONE => ()
				                          | SOME (n',p') => typeError p ("duplicate " ^ s
								                                             ^ " '" ^ (S.name n) ^ "'"
								                                             ^ " @" ^ E.getLinePos(p'))
				                      ; checkDups (s, nps)
                                  )
				  
  fun checkInt (T.INT, _,   _)   = ()
    | checkInt (ty,    pos, msg) = typeError pos ("int required in " ^ msg
						                                ^ ", got " ^ (unparseTy ty))
				                       
  fun checkBool (T.BOOL, _,   _)   = ()
    | checkBool (ty,    pos, msg) = typeError pos ("bool required in " ^ msg
						                                ^ ", got " ^ (unparseTy ty))
				                       
  fun checkUnit (T.UNIT, _,   _)   = ()
    | checkUnit (ty,     pos, msg) = typeError pos ("unit required in " ^ msg
						                                  ^ ", got " ^ (unparseTy ty))
				                         
  fun checkNotNil (T.NIL, pos, msg) = typeError pos ("illegal untyped nil in " ^ msg)
    | checkNotNil (_,     _,   _)   = ()
				      
  (* <, <=, >, >= *)
  fun checkComparable (T.INT,    T.INT,    _,   _)   = ()
    | checkComparable (T.BOOL,   T.BOOL,   _,   _)   = ()
    | checkComparable (T.STRING, T.STRING, _,   _)   = ()
    | checkComparable (ty,       ty',      pos, msg) = typeError pos ("incompatible types, "
								                                              ^ (unparseTy ty) ^ " and "
								                                              ^ (unparseTy ty') ^ ", in " ^ msg)
						                                     
  (* =, <>, :=, var *)
  fun checkAssignable (      T.NIL,                  T.NIL,            pos, msg) =
      typeError pos ("illegal untyped nil in " ^ msg)
    | checkAssignable (      T.RECORD (_, _),        T.NIL,            _,   _)   = ()
    | checkAssignable (      T.NIL,                  T.RECORD (_, _),  _,   _)   = ()
    | checkAssignable (ty as T.RECORD (_, u), ty' as T.RECORD (_, u'), pos, msg) =
      if u <> u'
		then typeError pos ("incompatible record types, "
								  ^ (unparseTy ty) ^ " and "
								  ^ (unparseTy ty') ^ ", in " ^ msg)
		else ()
    | checkAssignable (ty as T.ARRAY  (_, u), ty' as T.ARRAY  (_, u'), pos, msg) =
      if u <> u'
		then typeError pos ("incompatible array types, "
								  ^ (unparseTy ty) ^ " and "
								  ^ (unparseTy ty') ^ ", in " ^ msg)
		else ()
    | checkAssignable (ty,              ty',              pos, msg) = checkComparable (ty, ty', pos, msg)
								                                              
  fun morePreciseTy (ty as T.RECORD (_, _),       T.NIL)             = ty
    | morePreciseTy (      T.NIL,           ty as T.RECORD (_, _))   = ty
    | morePreciseTy (ty,                    _ )                      = ty
								                                               
  (* function return, IF consequents *)
  fun checkEqual (T.UNIT, T.UNIT, pos, msg) = ()
    | checkEqual (ty,     ty',    pos, msg) = checkAssignable (ty, ty', pos, msg)
					                               
  fun lookupRawTy (tenv, typ, pos, msg) = case S.look (tenv, typ)
					                            of NONE => ( typeError pos (msg ^ " type not defined: " ^ (S.name typ))
						                                    ; T.UNIT
                                                      )
					                             | SOME ty => ty
										                           
  fun actualTy (T.NAME (_, t)) = (case !t
				                       of NONE => ( E.impossible "TYPE NOT COMPLETED"
					                               ; T.UNIT
                                              )
				                        | SOME ty => let val ty = actualTy (ty)
						                               in t := SOME ty
						                                  ; ty
						                               end)
    | actualTy ty = ty
		              
  fun lookupTy (tenv, typ, pos, msg) = actualTy (lookupRawTy (tenv, typ, pos, msg))
				                           
  (* transTy : tenv * A.ty -> T.ty *)
  fun transTy (tenv, A.NameTy (typ, pos)) = lookupRawTy (tenv, typ, pos, "named")
					                             
    | transTy (tenv, A.RecordTy fs) =
      let fun doField {name,escape,typ,pos} = (name, lookupRawTy (tenv, typ, pos, "field"))
      in checkDups ("field name", map (fn {name,escape,typ,pos} => (name, pos)) fs)
		 ; T.RECORD ( map doField fs, ref ())
		end
    | transTy (tenv, A.ArrayTy (typ, pos)) = T.ARRAY (lookupRawTy (tenv, typ, pos, "array element"), ref ())
					                              
  (* transDec : (dec * (venv, tenv, level, break)) -> (venv, tenv, exp list) *)
  and transDec (A.VarDec{name,escape,typ,init,pos}, (venv, tenv, level, break)) =
      let val expty as {exp=expi,ty=tyi} = transExp (venv, tenv, level, break) init
			 val access = X.allocLocal level (!escape)
		in ( S.enter ( venv
						 , name
						 , case typ
						    of NONE => ( checkNotNil (tyi, pos, "inferred variable type")
										   ; V.VarEntry{ access = access
														   , ty = tyi
                                             }
                                 )
							  | SOME (typ, pos') => let val tyv = lookupTy (tenv, typ, pos', "variable")
														   in checkAssignable (tyi, tyv, pos, "variable initializer")
															 ; V.VarEntry{ access = access
																	       , ty = morePreciseTy (tyv, tyi)
                                                          }
														   end )
         , tenv
         , SOME (X.xlVarDec (access, level, expi))
         )
		end
										  
    | transDec (A.TypeDec nts, (venv, tenv, level, break)) =
      ( checkDups ("type", map (fn {name,ty,pos} => (name,pos)) nts)
		; let fun addOne ({name,ty,pos}, tenv) = S.enter (tenv, name, T.NAME (name, ref NONE))
            val tenv = foldl addOne tenv nts
            fun fixOne {name,ty,pos} = case S.look (tenv, name)
											       of SOME (T.NAME (_, t)) => t := SOME (transTy (tenv, ty))
											        | _ => E.impossible "MISSING TYPE INCONSISTENCY"
            fun checkOne {name,ty,pos} =
				    let val chainLen = (length nts)
                    fun walkTy (0, T.NAME (_, _)) = typeError pos ("cyclic type found at: " ^ (S.name name))
							 | walkTy (d, T.NAME (_, t)) = (case !t
                                                      of SOME ty => walkTy (d-1, ty)
                                                       | NONE => E.impossible ("INCOMPLETE TYPE " ^ (S.name name)))
							 | walkTy (_, ty) = ()
					 in walkTy (chainLen, lookupRawTy (tenv, name, pos, "recursive"))
                end
		  in app fixOne nts
			; app checkOne nts
			; (venv, tenv, NONE)
		  end
      )
					       
    | transDec (A.FunctionDec nfs, (venv, tenv, level, break)) =
      ( checkDups ("function", map (fn {name,params,result,body,pos} => (name, pos)) nfs)

		; let fun addFun ({name,params,result,body,pos}, venv) =
                let val l = M.newlabel()
					 in S.enter ( venv
									, name
									, V.FunEntry { level = X.newLevel { formals = map (fn {name,escape,typ,pos} => !escape) params
																				 , name = l
																				 , parent = level
                                                             }
                                        , label = l
													 , formals = map (fn {name,escape,typ,pos} => lookupTy (tenv, typ, pos, "argument")) params
													 , result = case result
																	 of NONE => T.UNIT
																	  | SOME (typ, pos) => lookupTy (tenv, typ, pos, "result")
                                        } )
					 end

            val declVenv = foldl addFun venv nfs

            fun addArgs ({name,escape,typ,pos}, tyf, venv) = S.enter ( venv
																							, name
																							, V.VarEntry { access = X.allocLocal level (!escape)
																											 , ty = tyf
                                                                                  } )

		      fun checkFun ({level,label,formals,result}, params, body, pos) =
                ( checkDups ("argument", map (fn {name,escape,typ,pos} => (name, pos)) params)
					 ; let val bodyVenv = (ListPair.foldlEq addArgs declVenv (params, formals)
												  handle ListPair.UnequalLengths => E.impossible "FUNCTION ARGS INCONSISTENCY")
						in let val exptyf as {exp=expb, ty=tyb} = transExp (bodyVenv, tenv, level, NONE) body
							in checkEqual (result, tyb, pos, "function return")
							 ; case result
								 of T.UNIT => X.xlUnitFunDec (level, expb)
								  | _ => X.xlFunDec (level, expb)
							end
						end )

            fun withFun {name,params,result,body,pos} = case S.look (declVenv, name)
														               of SOME (V.FunEntry ve) => checkFun (ve, params, body, pos)
														                | _ => E.impossible "MISSING FUNCTION INCONSISTENCY"
					                                                               
        in app withFun nfs
		   ; (declVenv, tenv, NONE)
		  end
      )
								 
  (* transExp : (venv, tenv, level, break) -> exp -> expty *)
  and transExp (venv, tenv, level, break) =
      (* trExp : exp -> expty *)
      let fun trExp A.NilExp = { exp = X.xlNil
			                      , ty = T.NIL
                               }
	         | trExp (A.IntExp i) = { exp = X.xlInt i
				                       , ty = T.INT
                                   }
            | trExp (A.BoolExp b) = { exp = X.xlBool b
                                    , ty = T.BOOL
                                    }
	         | trExp (A.StringExp (s, _)) = { exp = X.xlString s
					                            , ty = T.STRING
                                           }
				      
	         | trExp (A.OpExp{left,oper,right,pos}) =
              let val exptyl as {exp=expl,ty=tyl} = trExp left
						val exptyr as {exp=expr,ty=tyr} = trExp right
				  in case (opType oper)
                  of ARITHMETIC => ( checkInt (tyl, pos, "left operand to " ^ (U.opname oper))
							              ; checkInt (tyr, pos, "right operand to " ^ (U.opname oper))
							              ; { exp = X.xlArith (oper, expl, expr)
								             , ty = T.INT
                                     }
                                   )
                   | EQUALITY => ( checkAssignable (tyl, tyr, pos, U.opname oper)
								         ; { exp = case tyl
										              of T.INT => X.xlIntComp (oper, expl, expr)
                                             | T.BOOL => X.xlBoolComp (oper, expl, expr)
										               | T.STRING => X.xlStrComp (oper, expl, expr)
										               | T.RECORD _ => X.xlPtrEq (expl, expr)
										               | T.NIL => X.xlPtrEq (expl, expr)
										               | T.ARRAY _ => X.xlPtrEq (expl, expr)
										               | _ => X.xlNil
									        , ty = T.BOOL } 
                                 )
                   | COMPARISON => ( checkComparable (tyl, tyr, pos, U.opname oper)
								           ; { exp = case tyl
										                of T.INT => X.xlIntComp (oper, expl, expr)
										                 | T.STRING => X.xlStrComp (oper, expl, expr)
										                 | _ => X.xlNil
									          , ty = T.BOOL }
                                   )
				  end
						     
	         | trExp (A.IfExp{test,then',else'=NONE,pos}) =
              let val exptyt as {exp=expt, ty=tyt} = trExp test
						val exptyn as {exp=expn, ty=tyn} = trExp then'
				  in checkBool (tyt, pos, "IF test")
					; { exp = X.xlIfThen (expt, expn)
					  , ty = tyn }
				  end
	      
	         | trExp (A.IfExp{test,then',else'=SOME else',pos}) =
              let val exptyt as {exp=expt, ty=tyt} = trExp test
						val exptyn as {exp=expn, ty=tyn} = trExp then'
						val exptye as {exp=expe, ty=tye} = trExp else'
				  in checkBool (tyt, pos, "IF test")
					; checkEqual (tyn, tye, pos, "IF consequents")
					; { exp = X.xlIfThenElse (expt, expn, expe)
					  , ty = morePreciseTy (tyn, tye) }
				  end
	      
	    | trExp (A.WhileExp{test,body,pos}) =
         let val break = M.newlabel()
			in let val exptyt as {exp=expt, ty=tyt} = trExp test
					 val exptyb as {exp=expb, ty=tyb} = transExp (venv, tenv, level, SOME break) body
				in checkBool (tyt, pos, "WHILE test")
				 ; checkUnit (tyb, pos, "WHILE body")
				 ; { exp = X.xlWhile (expt, expb, break)
					, ty = T.UNIT }
				end
			end

       | trExp (A.ForExp _) = E.impossible "FOREXP NOT TRANSLATED"
         
	    | trExp (A.BreakExp pos) = (case break
					                      of NONE => ( typeError pos "break not in for/while"
						                              ; { exp = X.xlNil
						                                , ty = T.UNIT }
                                                )
					                       | SOME _ => { exp = X.xlBreak break
						                                , ty = T.UNIT })
				       
	    | trExp (A.SeqExp []) = { exp = X.xlNil
				                   , ty = T.UNIT }
	    | trExp (A.SeqExp [(exp, pos)]) = ( typeError pos "sequence of one expression"
					                          ; trExp exp )
	    | trExp (A.SeqExp ((exp,_)::exps)) =
         let fun oneExp ((exp,_), {exp=exps,ty=_}) = let val {exp,ty} = trExp exp
											                    in { exp = X.xlSeq (exps, exp)
											                       , ty = ty }
                                                     end
         in foldl oneExp (trExp exp) exps
         end
				
	    | trExp (A.VarExp var) = trVar var
				     
	    | trExp (A.AssignExp{var,exp,pos}) =
         let val exptyv as {exp=expv, ty=tyv} = trVar var
				 val exptye as {exp=expe, ty=tye} = trExp exp
			in checkAssignable (tyv, tye, pos, "assignment")
			 ; { exp = X.xlAssign(expv, expe)
				, ty = T.UNIT }
			end
						 
	    | trExp (A.CallExp{func,args,pos}) =
         let val exptys = map trExp args
			in case S.look (venv, func)
				 of SOME (V.FunEntry{level=level',label,formals,result}) =>
                let fun chkArg ({exp=_, ty=tya}, tyf) = checkAssignable (tya, tyf, pos, "function argument")
                in ( ListPair.appEq chkArg (exptys, formals)
						 ; { exp = X.xlCall (level, level', label, map (fn {exp, ty} => exp) exptys)
							, ty = result }
                   )
						 handle ListPair.UnequalLengths => ( typeError pos "wrong number of arguments"
																	  ; { exp = X.xlNil
																		 , ty = result })
                end
				  | _ => ( typeError pos ("undefined function: " ^ (S.name func))
			            ; { exp = X.xlNil
							  , ty = T.UNIT }
                     )
			end
			
	    | trExp (A.ArrayExp{typ,size,init,pos}) =
         let val exptyn as {exp=expn, ty=tyn} = trExp size
				 val exptyi as {exp=expi, ty=tyi} = trExp init
			in checkInt (tyn, pos, "array size")
			 ; case lookupTy (tenv, typ, pos, "array")
				 of tya as (T.ARRAY (tye, _)) => ( checkAssignable (actualTy tye, tyi, pos, "array element")
											            ; { exp = X.xlArray(expn, expi)
											              , ty = tya } )
				  | _ => ( typeError pos ("not an array type for creation: " ^ (S.name typ))
							; { exp = X.xlNil
							  , ty = T.ARRAY (tyi, ref ()) } )
			end
						      
	    | trExp (A.RecordExp{fields,typ,pos}) =
         let fun getFieldType (name', pos, [], nts') = ( typeError pos ("no such field: " ^ (S.name name'))
											                      ; (T.UNIT, nts'))
					| getFieldType (name', pos, (name, ty)::nts, nts') = if name' = name
													                             then (actualTy ty, nts@nts')
													                             else getFieldType (name', pos, nts, (name, ty)::nts')
			in case lookupTy (tenv, typ, pos, "record")
				 of tyr as T.RECORD (nts,_) => 
                let fun chkOneField (f as (name, {exp=_, ty=tya}, pos, fnum), (fs, nts)) =
                        let val (tyf, nts) = getFieldType (name, pos, nts, [])
								in checkAssignable (tya, tyf, pos, "record field")
									; ((f::fs), nts)
								end
                    val (_, fs) = foldl (fn ((name, exp, pos), (n, fs)) => (n+1, (name, trExp exp, pos, n)::fs))
													 (0, [])
													 fields
                in case foldl chkOneField ([], nts) fs
			           of (fs, []) => { exp = X.xlRecord (map (fn (_, {exp, ty}, _, fnum) => (exp, fnum)) fs)
								           , ty = tyr
                                   }
			            | (_, nts) => ( typeError pos ("uninitialized fields: " ^ (commaSep (map (fn (name,_) => S.name name) nts)))
								           ; { exp = X.xlNil
									          , ty = tyr
                                     }
                                   )
                end
			     | _ => ( typeError pos ("record type required: " ^ (S.name typ))
						   ; { exp = X.xlNil
						     , ty = T.RECORD ([], ref ())
                       }
                     )
         end
						    
	    | trExp (A.LetExp{decs,body,pos}) =
         let val (venv, tenv, exps) = foldl trDec (venv, tenv, []) decs
			in let val {exp, ty} = transExp (venv, tenv, level, break) body
				in { exp = X.xlLet(exps, exp)
					, ty=ty }
				end
			end
						
	  (* trDec : (dec, (venv, tenv, exp list) -> (venv, tenv, exp list) *)
	  and trDec (dec, (venv, tenv, exps)) =
         case transDec (dec, (venv, tenv, level, break))
			 of (venv, tenv, NONE) => (venv, tenv, exps)
			  | (venv, tenv, SOME exp) => (venv, tenv, exps@[exp])

	  (* trVar : var -> expty *)
	  and trVar (A.SimpleVar(id,pos)) =
         (case S.look (venv, id)
			  of SOME (V.VarEntry{access,ty}) => { exp = X.xlSimpleVar (access, level)
										                , ty = actualTy ty
                                              }
				| _ => ( typeError pos ("undefined variable: " ^ (S.name id))
						 ; { exp = X.xlNil
							, ty = T.UNIT
                     }
                   ))
			
	    | trVar (A.FieldVar(var,id,pos)) =
         let val exptyv as {exp=expv, ty=tyv} = trVar var
			in case tyv
				 of T.RECORD (sts,_) => let fun findField (_, []) = ( typeError pos ("record missing field: " ^ (S.name id))
												                            ; { exp = X.xlNil
													                           , ty = T.UNIT
                                                                  } )
										            | findField (n, (id',ty')::sts) = if id' = id
														                                  then { exp = X.xlField (expv, n)
														                                       , ty = actualTy ty'
                                                                                 }
														                                  else findField (n+1, sts)
									         in findField (0, sts)
									         end
				  | _ => ( typeError pos ("record required for field " ^ (S.name id))
							; { exp = X.xlNil
							  , ty = T.UNIT
                       } )
			end
					       
	    | trVar (A.SubscriptVar(var,sub,pos)) =
         let val exptyv as {exp=expv, ty=tyv} = trVar var
				 val exptyi as {exp=expi, ty=tyi} = trExp sub
			in checkInt (tyi, pos, "array subscript")
			 ; case tyv
				 of T.ARRAY (ty,_) => { exp = X.xlSubscript (expv, expi)
									       , ty = actualTy ty
                                  }
				  | _ => ( typeError pos "array required"
							; { exp = X.xlNil
							  , ty = T.UNIT
                       }
                     )
			end
      in trExp
      end
      
  (* A.exp -> X.frag list *)
  fun transProg exp = ( X.initResult ()
		                ; let val {exp=prog, ty=ty} = transExp (V.base_venv, V.base_tenv, X.outermost, NONE) exp
			               in TextIO.output (TextIO.stdErr, "program has type: " ^ (unparseTy ty)  ^ "\n")
			                ; case ty
			                   of T.UNIT => X.xlUnitFunDec (X.outermost, prog)
			                    | _ => X.xlFunDec (X.outermost, prog)
			               end
		                ; X.getResult ()
                      )
      end

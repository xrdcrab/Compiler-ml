structure Semant : SEMANT =
struct
  structure A = Absyn
  structure E = ErrorMsg
  structure M = Temp
  structure R = Tree
  structure S = Symbol
  structure T = Types
  structure V = Env
  structure X = Translate
		
  fun typeError pos msg = E.error pos ("type error: " ^ msg)
			  
  type expty = { exp : X.exp
	            , ty : T.ty
               }

  fun commaSep [] = ""
    | commaSep [s] = s
    | commaSep (s::ss) = s ^ ", " ^ (commaSep ss)
	       
  fun unparseTy T.INT = "int"
    | unparseTy T.STRING = "string"
    | unparseTy T.BOOL = "bool"
    | unparseTy T.UNIT = "unit"
    | unparseTy T.NIL = "nil"
    | unparseTy (T.NAME (name, ref NONE)) = "unknown type " ^ (S.name name)
    | unparseTy (T.NAME (name, ref (SOME _))) = S.name name
    | unparseTy (T.ARRAY (ty, _)) = "[" ^ (unparseTy ty) ^ "]"
    | unparseTy (T.RECORD (sts, _)) = "{"
				                          ^ (commaSep (map (fn (name, ty) => (S.name name)
									                                              ^ ":"
									                                              ^ (unparseTy ty)) sts))
				                          ^ "}"
	
  fun checkComparable (result, result', pos) = case (result, result') of
                                                 ({exp, ty = T.INT }, {exp = _, ty = T.INT}) =>
                                                   ()
                                               | ({exp, ty = T.STRING }, {exp = _, ty = T.STRING}) =>
                                                   ()
                                               | ({exp, ty = T.BOOL }, {exp = _, ty = T.BOOL}) =>
                                                   ()
                                               | _ =>
                                                 (E.error pos "types are not comparable")

  (* <, <=, >, >= *)
  fun checkComparable (ty, ty', pos, msg) = typeError pos ("incompatible types, "
							                                      ^ (unparseTy ty) ^ " and "
							                                      ^ (unparseTy ty') ^ ", in " ^ msg)
						       
  (* =, <>, :=, var *)
  fun checkComparable (ty, ty', pos, msg) = typeError pos ("incompatible types, "
							                                      ^ (unparseTy ty) ^ " and "
							                                      ^ (unparseTy ty') ^ ", in " ^ msg)
								      
  fun morePreciseTy (ty, ty') = ty
								       

  fun lookupRawTy (tenv, typ, pos, msg) = case S.look (tenv, typ)
					                            of NONE => ( typeError pos (msg ^ " type not defined: " ^ (S.name typ))
						                                    ; T.UNIT )
					                             | SOME ty => ty

  fun actualTy (T.NAME (_, t)) = (case !t
				                       of NONE => ( E.impossible "TYPE NOT COMPLETED"
					                               ; T.UNIT )
				                        | SOME ty => let val ty = actualTy (ty)
						                               in ( t := SOME ty
						                                  ; ty )
						                               end)
    | actualTy ty = ty
		    
  fun lookupTy (tenv, typ, pos, msg) = actualTy (lookupRawTy (tenv, typ, pos, msg))
				       
  (* transTy : tenv * A.ty -> T.ty *)
  fun transTy (tenv, texp) = case texp
                              of _ => T.UNIT
					     
  (* transDec : (venv, tenv, level, break) dec -> (venv, tenv, exp list) *)
  and transDec (venv, tenv, level, break) _ = (venv, tenv, level, break)
								 
  (* transExp : (venv, tenv, level, break) -> exp -> expty *)
  and transExp (venv, tenv, level, break) exp =

      (* trExp : exp -> expty *)
      let fun trExp _ = { ty = T.UNIT, exp = X.Ex (R.CONST 0) }

      (* trDec : (venv, tenv, exp list) -> dec -> (venv, tenv, exp list) *)
      and trDec (venv, tenv, exps) _ = (venv, tenv, exps)

      (* trVar : var -> expty *)
      and trVar _ = { ty = T.UNIT, exp = X.Ex (R.CONST 0) }

      in trExp exp
      end
      
  (* A.exp -> X.frag list *)
  fun transProg exp = ( X.initResult ()
		                ; let val {exp=prog, ty=ty} = transExp (V.base_venv, V.base_tenv, X.outermost, NONE) exp
			               in ( TextIO.output (TextIO.stdErr, "program has type: " ^ (unparseTy ty)  ^ "\n")
			                  ; case ty
			                     of _ => X.Ex (R.CONST 0))
			               end
		                ; X.getResult () )
end

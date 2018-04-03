signature ENV =
sig
    type ty

    datatype enventry = VarEntry of { access : Translate.access
                                    , ty : ty
                                    }
		      | FunEntry of { level : Translate.level
                                    , label : Temp.label
                                    , formals : ty list
                                    , result : ty
                                    }

    type tenv = ty Symbol.table
    type venv = enventry Symbol.table

    val base_tenv : tenv
    val base_venv : venv
end

structure Env : ENV =
struct
    structure M = Temp
    structure S = Symbol
    structure T = Types
    structure X = Translate

    type ty = T.ty

    datatype enventry = VarEntry of { access : X.access
                                    , ty : ty
                                    }
		      | FunEntry of { level : X.level
                                    , label : M.label
                                    , formals : ty list
                                    , result : ty
                                    }
                                    
    type tenv = ty S.table
    type venv = enventry S.table

    val base_venv = S.empty;  (* fill this in *)
		    
    val base_tenv = S.empty;  (* fill this in *)
end

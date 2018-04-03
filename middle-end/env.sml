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


    val base_venv =  
    (* add predefined functions to 0 *)
        S.enter(
        S.enter(
        S.enter(
        S.enter(
        S.enter(
        S.enter(
        S.enter(
        S.enter(
        S.enter(
        S.enter(S.empty, 
            S.symbol("print"),   FunEntry {level=X.outermost, label=M.namedlabel("print"), formals=[T.STRING], result=T.UNIT}),
            S.symbol("flush"),   FunEntry {level=X.outermost,label=M.namedlabel("flush"),formals=[],result=T.UNIT}),
            S.symbol("getchar"), FunEntry {level=X.outermost,label=M.namedlabel("getchar"),formals=[],result=T.STRING}),
            S.symbol("ord"), FunEntry {level=X.outermost, label=M.namedlabel("ord"),formals=[T.STRING],result=T.INT}),
            S.symbol("chr"), FunEntry {level=X.outermost,label=M.namedlabel("chr"),formals=[T.INT],result=T.STRING}),  
            S.symbol("size"), FunEntry {level=X.outermost,label=M.namedlabel("size"),formals=[T.STRING],result=T.INT}),  
            S.symbol("substring"), FunEntry {level=X.outermost,label=M.namedlabel("substring"),formals=[T.STRING,T.INT,T.INT],result=T.STRING}),  
            S.symbol("concat"), FunEntry {level=X.outermost,label=M.namedlabel("concat"),formals=[T.STRING,T.STRING],result=T.STRING}),  
            S.symbol("not"), FunEntry {level=X.outermost,label=M.namedlabel("not"),formals=[T.INT],result=T.BOOL}),  
            S.symbol("exit"), FunEntry {level=X.outermost,label=M.namedlabel("exit"),formals=[T.INT],result=T.UNIT})

    val base_tenv =
    (* add predefined types to 0*)
            S.enter(
                     S.enter(
                        S.enter(S.empty, S.symbol("int"), T.INT), S.symbol("string"), T.STRING), S.symbol("bool"), T.BOOL)

end

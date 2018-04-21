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
                
    val base_tenv =
        let fun enter ((name, ty), tenv) = S.enter ( tenv
                                                   , S.symbol name
                                                   , ty
                                                   )
        in foldl enter S.empty [ ("bool",   T.BOOL)
                               , ("int",    T.INT)
                   , ("string", T.STRING)
                   , ("unit",   T.UNIT)
                               ]
        end

    val base_venv =
        let fun enter ((name, formals, result), venv) = S.enter ( venv
                                                                , S.symbol name
                                                                , FunEntry { level = X.outermost
                                       , label = M.namedlabel name
                                       , formals = formals
                                       , result = result
                                                                           }
                                                                )
        in foldl enter S.empty [ ("print",     [T.STRING],               T.UNIT)
                   , ("flush",     [],                       T.UNIT)
                   , ("getchar",   [],                       T.STRING)
                   , ("ord",       [T.STRING],               T.INT)
                   , ("chr",       [T.INT],                  T.STRING)
                   , ("size",      [T.STRING],               T.INT)
                   , ("substring", [T.STRING, T.INT, T.INT], T.STRING)
                   , ("concat",    [T.STRING, T.STRING],     T.STRING)
                   , ("not",       [T.BOOL],                 T.BOOL)
                   , ("exit",      [T.INT],                  T.UNIT)
                               ]
        end
end

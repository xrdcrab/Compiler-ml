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
  
  (* <, <=, >, >= *)
  (* if comaprable -> true, false otherwise*)
  fun checkComparable (ty, ty', pos) = case (ty, ty') 
                                           of (T.INT, T.INT) => true
                                            | (T.STRING, T.STRING) => true
                                            | (T.BOOL, T.BOOL) => true
                                            | _ => (E.error pos ("types are not comparable");false)


  (* <, <=, >, >= *)
  (* fun checkComparable (ty, ty', pos, msg) = typeError pos ("incompatible types, "
                                                    ^ (unparseTy ty) ^ " and "
                                                    ^ (unparseTy ty') ^ ", in " ^ msg) *)
                   
  (* =, <>, :=, var *)
  (* fun checkComparable (ty, ty', pos, msg) = typeError pos ("incompatible types, "
                                                    ^ (unparseTy ty) ^ " and "
                                                    ^ (unparseTy ty') ^ ", in " ^ msg) *)

  (* =, <>, := *)
  (* if assinable -> true, false otherwise *)
  fun checkAssignable (ty, ty', pos) = case (ty, ty') 
                                           of (T.INT, _) => checkComparable (ty, ty', pos)
                                            | (T.STRING, _) => checkComparable (ty, ty', pos)
                                            | (T.BOOL, _) => checkComparable (ty, ty', pos)
                                            | (T.RECORD(_, u), T.RECORD(_, u')) =>
                                                if u = u' 
                                                then true
                                                else false
                                            | (T.RECORD(_, _), T.NIL) => true
                                            | (T.NIL,  T.RECORD(_, _)) => true
                                            | (T.ARRAY (_, u), T.ARRAY (_, u')) =>
                                                if u = u'
                                                then true
                                                else false
                                            | _ => false

  fun isInt({exp,ty}, pos) = if ty=T.INT then true else false


  fun checkInt({ exp, ty },pos) = if ty = T.INT then ()
                                else typeError pos "int required"

  fun checkBool({ exp, ty },pos) = if ty = T.BOOL then ()
                                 else typeError pos "bool required"

  fun checkString({ exp, ty }, pos) = if ty = T.STRING then ()
                                    else typeError pos "string required"

  fun checkUnit({ exp, ty }, pos) = if ty = T.UNIT then ()
                                  else typeError pos "unit required"

  fun checkArray(ty, {exp, ty1},pos) = if checkAssignable(ty,ty1,pos) then ()
                                else typeError pos "array required"
  fun checkRecord(ty, {exp, ty1},pos) = if checkAssignable(ty,ty1,pos) then ()
                                else typeError pos "record required"

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
  fun transTy (tenv, texp) = (case texp of
                                   A.NameTy (name,pos) => lookupRawTy(tenv,name,pos,"name") 
                                 | A.ArrayTy(name,pos) => (case S.look(tenv,name) of 
                                                                NONE => (typeError pos "array type unknown"; T.UNIT)
                                                              | SOME(ty) => T.ARRAY(ty, ref()))
                                 | A.RecordTy(fields) => let
                                                             fun findFieldTy({name,escape,typ,pos},fields') = 
                                                              (case S.look(tenv,typ) of 
                                                                    NONE => (typeError pos "field type unknown"; (name,T.UNIT)::fields')
                                                                  | SOME(ty) => (name,ty)::fields')
                                                         in 
                                                             T.RECORD((foldr findFieldTy [] fields), ref())
                                                         end)




   (* transDec : (venv, tenv, level, break) dec -> (venv, tenv, exp list) *)
  and transDec (venv,tenv,level,break) dec =
    let 
            (* trDec : (venv, tenv, exp list) -> dec -> (venv, tenv, exp list) *)

      fun     
        trDec (venv, tenv, exps) dec =
   case dec of 
    A.VarDec{name, escape, typ, init, pos} =>
    let 
      val access1 = X.allocLocal level (!escape)
    in
        let
            val {exp=eExp,ty=ty} = transExp(venv,tenv,level,break) init
            val varExp =  X.simpleVar(access1,level) 
        in 
          case typ of 
            (* short *)
            NONE => 
              (if ty = T.NIL then (typeError pos "t =/= nil" ;{tenv=tenv, venv=S.enter(venv,name,V.VarEntry{access=access1, ty=ty}), expList=[X.error]})
                
              else {tenv=tenv, venv=S.enter(venv,name,V.VarEntry{access=access1, ty=ty}), expList=[X.assign(varExp,eExp)]} 
 (* ... *)              )
            (* long *)
            | SOME((name1,pos)) =>
                  case S.look (tenv, name1) of 
                   NONE => ((typeError pos "type");
                        {tenv=tenv, venv=S.enter(venv,name,V.VarEntry{access=access1, ty=ty}),expList=[X.error]})
                  | SOME(ty1) =>
                      (let 
   (* ... *)                val ty2 = actualTy(ty1)
                      in
                           (if checkAssignable(ty,ty1,pos) then {tenv=tenv, venv=S.enter(venv,name,V.VarEntry{access=access1,ty=ty2}),expList=[X.assign(varExp,eExp)]}
                           else (typeError pos "VarDec"; {tenv=tenv, venv=S.enter(venv,name,V.VarEntry{access=access1,ty=ty2}),expList=[X.error]}))
                                
                      end)
        end
      end

      |A.TypeDec[{name,ty,pos}] =>
        {venv=venv, tenv = S.enter(tenv,name,transTy(tenv,ty)), expList=[]}

        (*
      |A.FunctionDec[{name,params,result=SOME(rt,pos1),body,pos}] =>
        let 
          val SOME(result_ty) = S.look(tenv,rt)
          fun transparam{name,escape,typ,pos} =
                  case S.look(tenv,typ) of
                    SOME t => {name=name, ty=t}
          val params' = map transparam params
          val venv' = S.enter(venv,name,V.FunEntry{level=level, label=name, formals = map #ty params', result=result_ty})
          fun enterparam ({name,ty},venv) =
              S.enter(venv,name,V.VarEntry{access=(),ty=ty})
          val venv'' = foldl enterparam params' venv'
        in transExp(venv'',tenv) body;
          {venv=venv',tenv=tenv, expList=[]}
        end 
        *)
      in
        trDec
      end


  (* transExp : (venv, tenv, level, break) -> exp -> expty *)
  and transExp (venv, tenv, level, break) exp=

      (* trExp : exp -> expty *)
      let
      fun trExp A.NilExp = {ty = T.NIL,exp = X.nilExp}
      |trExp (A.BoolExp bool) = {exp = X.boolExp(bool), ty = T.BOOL}
      |trExp (A.IntExp int) = {exp=X.intExp(int),ty = T.INT}
      |trExp (A.StringExp (string,pos)) = {exp=X.stringExp(string), ty = T.STRING}
      |trExp (A.OpExp {left, oper=A.PlusOp,right,pos})=
        let
            val {exp=left1,ty=leftTy} = trExp left
            val {exp=right1,ty=rightTy} = trExp right
        in
            (checkInt({exp=left1,ty=leftTy},pos); checkInt({exp=right1,ty=rightTy},pos); {exp=X.opers(left1,A.PlusOp,right1), ty=T.INT})
        end
      |trExp (A.OpExp {left, oper=A.MinusOp,right,pos})=
        let
            val {exp=left1,ty=leftTy} = trExp left
            val {exp=right1,ty=rightTy} = trExp right
        in
            (checkInt({exp=left1,ty=leftTy},pos); checkInt({exp=right1,ty=rightTy},pos); {exp=X.opers(left1,A.MinusOp,right1), ty=T.INT})
        end
      |trExp (A.OpExp {left, oper=A.TimesOp,right,pos})=
        let
            val {exp=left1,ty=leftTy} = trExp left
            val {exp=right1,ty=rightTy} = trExp right
        in
            (checkInt({exp=left1,ty=leftTy},pos); checkInt({exp=right1,ty=rightTy},pos); {exp=X.opers(left1,A.TimesOp,right1), ty=T.INT})
        end
      |trExp (A.OpExp {left, oper=A.DivideOp,right,pos})=
        let
            val {exp=left1,ty=leftTy} = trExp left
            val {exp=right1,ty=rightTy} = trExp right
        in
            (checkInt({exp=left1,ty=leftTy},pos); checkInt({exp=right1,ty=rightTy},pos); {exp=X.opers(left1,A.DivideOp,right1), ty=T.INT})
        end
        (* =,<> if assinable->bool *)
      |trExp (A.OpExp {left, oper=A.EqOp,right,pos})=
        let
            val {exp=leftExp, ty=leftTy} = trExp left
            val {exp=rightExp, ty=rightTy} = trExp right
        in
            (if checkAssignable(leftTy,rightTy,pos) then (
              if isInt({exp=leftExp,ty=leftTy},pos) andalso isInt({exp=rightExp,ty=rightTy},pos)  then {exp=X.opers(leftExp,A.EqOp,rightExp), ty=T.BOOL}
              else {exp=X.stringOpers(leftExp,A.EqOp,rightExp), ty=T.BOOL}
                )
            else (E.error pos "Type missmatch in EqOp"; {exp=X.error, ty=T.UNIT}))
        end
      |trExp (A.OpExp {left, oper=A.NeqOp,right,pos})=
        let
            val {exp=leftExp, ty=leftTy} = trExp left
            val {exp=rightExp, ty=rightTy} = trExp right
        in
            (if checkAssignable(leftTy,rightTy,pos) then (
              if isInt({exp=leftExp,ty=leftTy},pos)  andalso isInt({exp=rightExp,ty=rightTy},pos) then {exp=X.opers(leftExp,A.NeqOp,rightExp), ty=T.BOOL}
              else {exp=X.stringOpers(leftExp,A.NeqOp,rightExp), ty=T.BOOL}
              )
            else (E.error pos "Type missmatch in NeqOp"; {exp=X.error, ty=T.UNIT}))        
        end
        (* <,<=,>,>= if comparable -> bool *)
      |trExp (A.OpExp {left, oper=A.LtOp,right,pos})=
        let
            val {exp=leftExp, ty=leftTy} = trExp left
            val {exp=rightExp, ty=rightTy} = trExp right
        in
            (if checkComparable(leftTy,rightTy,pos) then (
              if isInt({exp=leftExp,ty=leftTy},pos)  andalso isInt({exp=rightExp,ty=rightTy},pos) then {exp=X.opers(leftExp,A.LtOp,rightExp), ty=T.BOOL}
              else {exp=X.stringOpers(leftExp,A.LtOp,rightExp), ty=T.BOOL}
            )
            else (E.error pos "Type missmatch in LtOp"; {exp=X.error, ty=T.UNIT}))
        end
      |trExp (A.OpExp {left, oper=A.LeOp,right,pos})=
        let
            val {exp=leftExp, ty=leftTy} = trExp left
            val {exp=rightExp, ty=rightTy} = trExp right
        in
            (if checkComparable(leftTy,rightTy,pos) then (
              if isInt({exp=leftExp,ty=leftTy},pos)  andalso isInt({exp=rightExp,ty=rightTy},pos) then {exp=X.opers(leftExp,A.LeOp,rightExp), ty=T.BOOL}
              else {exp=X.stringOpers(leftExp,A.LeOp,rightExp), ty=T.BOOL}
              )
            else (E.error pos "Type missmatch in LeOp"; {exp=X.error, ty=T.UNIT}))
        end
      |trExp (A.OpExp {left, oper=A.GtOp,right,pos})=
        let
            val {exp=leftExp, ty=leftTy} = trExp left
            val {exp=rightExp, ty=rightTy} = trExp right
        in
            (if checkComparable(leftTy,rightTy,pos) then (
          if isInt({exp=leftExp,ty=leftTy},pos) andalso isInt({exp=rightExp,ty=rightTy},pos)  then {exp=X.opers(leftExp,A.GtOp,rightExp), ty=T.BOOL}
              else {exp=X.stringOpers(leftExp,A.GtOp,rightExp), ty=T.BOOL}
              )
            else (E.error pos "Type missmatch in GtOp"; {exp=X.error, ty=T.UNIT}))
        end
      |trExp (A.OpExp {left, oper=A.GeOp,right,pos})=
        let
            val {exp=leftExp, ty=leftTy} = trExp left
            val {exp=rightExp, ty=rightTy} = trExp right
        in
            (if checkComparable(leftTy,rightTy,pos) then (
            if isInt({exp=leftExp,ty=leftTy},pos) andalso isInt({exp=rightExp,ty=rightTy},pos)  then {exp=X.opers(leftExp,A.GeOp,rightExp), ty=T.BOOL}
              else {exp=X.stringOpers(leftExp,A.GeOp,rightExp), ty=T.BOOL}
              )
            else (E.error pos "Type missmatch in GeOp"; {exp=X.error, ty=T.UNIT}))
        end

        (* test:bool, then:unit -> unit *)
      |trExp (A.IfExp {test,then',else'=NONE,pos}) =
        let
            val {exp=test1,ty=testTy} = trExp test
            val {exp=then1,ty=thenTy} = trExp then'
        in
            (checkBool({exp=test1,ty=testTy},pos); checkUnit({exp=then1,ty=thenTy},pos) ; {exp=X.ifExp(test1,then1), ty=T.UNIT})
        end

        (* test:bool, assinable(then,else) => morePrecise *)
      |trExp (A.IfExp {test,then',else'=SOME expElse, pos})=
        let
            val {exp=testExp,ty=testTy} = trExp test
            val {exp=thenExp,ty=thenTy} = trExp then'
            val {exp=elseExp,ty=elseTy} = trExp expElse
        in
            (checkBool({exp=testExp,ty=testTy},pos); 
              if checkAssignable(thenTy, elseTy ,pos) then {exp=X.ifElse(testExp,thenExp,elseExp),ty=morePreciseTy(thenTy,elseTy)}
            else (E.error pos "Type missmatch in IfExp"; {exp=X.error, ty=T.UNIT}))
        end

        (* test:bool, body:unit -> unit *)
      |trExp (A.WhileExp {test,body,pos}) =
        let
            val breakPos = X.newBreakPos()
            val {exp=testExp,ty=testTy} = trExp test
            val {exp=bodyExp,ty=bodyTy} = trExp body

        in
            checkBool({exp=testExp,ty=testTy},pos); checkUnit({exp=bodyExp,ty=bodyTy},pos); {exp=X.whileExp(testExp,bodyExp,breakPos), ty=T.UNIT}
        end

        (* lo:int, hi:int, body:unit -> unit *)
      |trExp (A.ForExp {var,escape,lo,hi,body,pos}) =
        let
            val breakPos = X.newBreakPos()
            val {exp=loExp,ty=loTy} = trExp lo
            val {exp=hiExp,ty=hiTy} = trExp hi
            val {exp=bodyExp,ty=bodyTy} = trExp body
            val access = X.allocLocal level (!escape)
            
        in
            (checkInt({exp=loExp,ty=loTy},pos);
              checkInt({exp=hiExp,ty=hiTy},pos);
              checkUnit({exp=bodyExp,ty=bodyTy},pos);
               {exp=X.for(X.simpleVar(access,level),breakPos,loExp,hiExp,bodyExp),ty=T.UNIT})
        end


      (*  type=last expression in seq *)        
      |trExp (A.SeqExp (explist)) = 
        let 
            val (translatedExps, ty) = (foldl (fn ((exp, _), (exps, _)) =>
                                                            let 
                                                              val {exp=anExp,ty} = trExp(exp)
                                                          in (exps@[anExp], ty) 
                                                          end)
                                                             ([], T.UNIT)
                                                          explist)
                                    in
                                        {exp=X.seqExp(translatedExps), ty=ty}
                                    end
      |trExp (A.BreakExp pos) = (case break of 
          NONE => ((typeError pos "break not allowed"); {exp=X.error,ty=T.UNIT})
         |SOME(breakPos) => {exp=X.break(breakPos),ty=T.UNIT})

      |trExp (A.RecordExp {fields,typ,pos}) = (case S.look(tenv, typ)
                                               of NONE => (E.error pos ("Record not defined"); {exp=X.error,ty=T.UNIT})
                                                | SOME(T.RECORD(fields',unique)) => 
                                                   (let 
                                                      val fieldsLen = length fields
                                                      val fieldsLen' = length fields'
                                                    in
                                                      (if fieldsLen <> fieldsLen' then 
                                                          (E.error pos ("fields length not match"); {exp=X.error, ty=T.UNIT})
                                                      else
                                                          let 
                                                              fun checkMatch((symbl, fieldExp, pos), (symbl', ty')) = 
                                                                  let
                                                                    val {exp = fieldExp', ty = fieldTy} = trExp fieldExp
                                                                  in 
                                                                    ( if checkAssignable(ty', fieldTy,pos) then ()
                                                                      else
                                                                          (if symbl = symbl'
                                                                          then 
                                                                              (E.error pos ("Fields type not match"))
                                                                          else 
                                                                              (E.error pos ("Two fields have different symbol")));
                                                                      fieldExp')
                                                                  end
                                                              val fieldExps = ListPair.map checkMatch(fields, fields')
                                                          in 

                                                              {exp=X.record(fieldExps),ty=T.RECORD(fields', unique)}
                                                          end)
                                                    end)
                                                | SOME _ => (E.error pos ("Record not found"); {exp=X.error,ty=T.UNIT})
                                                   )

      (* size:int ->type of rawType typ *)
      |trExp (A.ArrayExp {typ,size,init,pos})=
        let
            val typTy = lookupRawTy (tenv,typ,pos,"ArrayExp")
            val {exp=sizeExp,ty=sizeTy} = trExp size
            val {exp=iExp,ty=iTy} = trExp init
        in
            (checkInt({exp=sizeExp,ty=sizeTy},pos);
              (case typTy of
                T.ARRAY(ty,_) => if checkAssignable(ty,iTy,pos) then {exp=X.arrayExp(sizeExp,iExp), ty=typTy}
                                else (typeError pos "array type incorrect";{exp=X.error,ty=T.UNIT})))
                
        end
        (* if assinable -> unit *)
      |trExp (A.AssignExp {var,exp,pos}) =
            let
                val {exp=vExp,ty=vTy} = trVar var
                val {exp=eExp,ty=eTy} = trExp exp
            in
                (if checkAssignable(vTy,eTy,pos) then {exp=X.assign(vExp,eExp),ty = T.UNIT}
                else (typeError pos "not assignable";  {exp=X.error, ty=T.UNIT}))
                 
            end
      |trExp (A.VarExp var) = trVar var

      |trExp (A.CallExp {func,args,pos}) = 
        (case S.look(venv, func) of 
            NONE => (E.error pos ("Function not defined"); {exp=X.error,ty=T.UNIT})
          | SOME(V.VarEntry _) => (E.error pos ("Variable is not a function");{exp=X.error, ty=T.UNIT})
          | SOME(V.FunEntry {level=funcLevel, label, formals, result = resultTy}) => 
                 (let 
                      val formalsLen = length formals
                      val argsLen = length args
                  in
                     (if argsLen <> formalsLen then (E.error pos ("arguments length not match"); {exp=X.error, ty=T.UNIT})
                      else
                           let 
                               fun checkMatch ((arg, formal), baseTy) = 
                                  let 
                                      val {exp = argExp, ty = actualTy} = trExp arg
                                      val equals = checkAssignable(actualTy, formal,pos)
                                  in
                                      equals::baseTy
                                  end
                               fun transArgs(arg) = 
                                  let
                                      val {exp = argExp, ty = argTy} = trExp arg
                                  in 
                                      argExp
                                  end

                               val equalOrNot = foldr checkMatch [] (ListPair.zip(args,formals))
                               val argTreeExps = map transArgs args
                          in 
                              (if List.all (fn a => a) equalOrNot
                               then {exp=X.callExp(label, argTreeExps, level, funcLevel), ty=resultTy}
                               else (E.error pos ("found argument type do not match "); {exp=X.error, ty=T.UNIT}))
                          end)
                    end))

        |trExp(A.LetExp {decs,body,pos}) = {ty = T.NIL,exp = X.nilExp}

(*
|trExp (A.LetExp {decs,body,pos}) =
        let
            val {venv=venv', tenv=tenv', exp=eExp} = transDec(venv,tenv,level,break) decs
        in
            transExp(venv',tenv',level,break) body
        end

  *)



                
      (* trVar : var -> expty *)
and trVar (A.SimpleVar(id,pos))=
        (case S.look(venv,id) of 
            SOME(V.VarEntry{access,ty}) => {exp=X.simpleVar(access,level),ty=actualTy(ty)}
            | NONE   => (typeError pos ("undefined variable " ^ S.name id);
              {exp=X.error, ty=T.INT}))
|trVar (A.FieldVar(var,id,pos))=
    let 
        val {exp,ty} = trVar var
    in 
        case ty of
          T.RECORD(list,_) =>
            let 
              val symbols = map #1 list
            in
                  {exp=(X.fieldVar(exp,id,symbols)), ty= lookupRawTy(tenv,id,pos,"fieldVar")}
            end
    end

  (* exp:int -> type of var's subcript*)
  |trVar (A.SubscriptVar(var,exp,pos))=
        let
            val {exp=vExp,ty} = trVar var
            val {exp=eExp,ty=expTy} = trExp exp
        in
           case ty of
            T.ARRAY(subTy,_) =>
                  (checkInt({exp=eExp,ty=expTy},pos); {exp=X.subscriptVar(vExp,eExp),ty=subTy})
        end


      in 
        trExp exp
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

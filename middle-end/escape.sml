structure FindEscape : sig
    val findEscape : Absyn.exp -> unit 
end =
struct
   structure A = Absyn
   structure E = ErrorMsg
   structure S = Symbol

   type depth = int
   type escEnv = (depth * bool ref) S.table


      fun traverseVar(env:escEnv, d:depth, A.SimpleVar(symbol, pos)) : unit =
         (*set escape to true if depth > d *)
                  (case S.look(env,symbol) of
                     SOME(depth,escape) => if depth>d then escape := true else ()
                        |NONE => ())
         |traverseVar(env:escEnv, d:depth,A.FieldVar(A.SimpleVar(symbol,pos), symbol1, pos1)) : unit =
                  (case S.look(env, symbol) of
                     SOME(depth,escape) => if depth>d then escape := true else ()
                        |NONE => ())
         |traverseVar(env:escEnv, d:depth,A.SubscriptVar(A.SimpleVar(symbol,pos),exp,pos1)):unit =
                  (case S.look(env, symbol) of 
                     SOME(depth,escape) => if depth>d then escape := true else ()
                        |NONE => ())


   	and traverseExp(env:escEnv, d:depth, s:A.exp) : unit =
         (* set escape of expression and variables *)
   			case s of
   				A.VarExp(var) => traverseVar(env,d,var)
   				|A.NilExp =>()
   				|A.IntExp(int) => ()
   				|A.BoolExp(bool) => ()
   				|A.StringExp(string, pos) => ()
   				|A.CallExp({func,args,pos}) => 
               (* set for each expression in args list *)
                     (List.map(fn(exp) => traverseExp(env,d,exp)) args;())
   				|A.OpExp({left,oper,right,pos}) =>
   						(traverseExp(env,d,left);(traverseExp (env,d,right)))
   				|A.RecordExp({fields, typ,pos}) => 
               (* set for each expression in fields list*)
                     (List.map(fn(symbol,exp,pos) => traverseExp(env,d,exp)) fields;())
   				|A.SeqExp(expList) =>     
               (* set for each expression in expression list *)
                     (List.map(fn(exp,pos) => traverseExp(env,d,exp)) expList;())
   				|A.AssignExp({var,exp,pos}) => 
   						(traverseVar(env,d,var);(traverseExp(env,d,exp)))
   				|A.IfExp({test,then',else',pos}) =>
   						(traverseExp(env,d,test);(traverseExp(env,d,then');
                     (* option *) 
   							case else' of
   								NONE => ()
   								|SOME(else1) => (traverseExp(env,d,else1))))
   				|A.WhileExp({test,body,pos}) => 
   						(traverseExp(env,d,test);(traverseExp(env,d,body)))
               |A.ForExp({var,escape,lo,hi,body,pos}) =>
               (* escape *)
                        let
                           val envWithVar = S.enter(env,var,(d,escape))
                        in
                           escape:=false;traverseExp(env,d,lo);traverseExp(env,d,hi);traverseExp(envWithVar,d,body)
                        end
   				|A.BreakExp(pos) =>()
   				|A.LetExp({decs,body,pos}) => 
               (* set for body expression for the enviroment after let declraction  *)
                     let 
                        val envAfterDec = traverseDecs(env,d,decs)
                     in
                        traverseExp(envAfterDec,d,body)
                     end
   				|A.ArrayExp({typ,size,init,pos}) => 
   						(traverseExp(env,d,size);(traverseExp(env,d,init)))

   	and traverseDecs(env, d, s:A.dec list): escEnv =
         let
            fun dOnList(decList,env1) = 
            List.foldl(fn (de,en) =>               
               case de of 
                  (* set for ty->recordTy.field->escape *)
                  A.TypeDec(decs1) => en
                  (*
                     case decs1 of
                        SOME(name,ty,pos) =>
                        case ty of
                           SOME(A.RecordTy(fields)) =>
                              List.map(fn(name,escape,typ,pos) => escape := false) fields
                              *)

                  |A.VarDec({name,escape,typ,init,pos}) => 
                     (escape := false; traverseExp(env,d,init); S.enter(env,name,(d,escape)))
|A.FunctionDec(fundecList) => en
                  (*List.map(fn(name,params,result,body,pos) => traverseExp(env,d,body)) fundecList;()
                 *) ) env1 decList
         in 
            dOnList (s,env)
         end





   val findEscape : (A.exp -> unit) = fn (prog) => traverseExp(S.empty,0,prog);	(* fill this in *)




end

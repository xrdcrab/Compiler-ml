structure FindEscape : sig
    val findEscape : Absyn.exp -> unit 
end =
struct
   structure A = Absyn
   structure E = ErrorMsg
   structure S = Symbol

   val findEscape : (A.exp -> unit) = fn _ => ();	(* fill this in *)
end

(* a simple assertion facility for SML *)

(* EXCEPTION *)
exception Assert of string;

(* assert -- raise an exception if an expression is false *)
val assert : bool * string -> unit =
 fn (false, s) => raise (Assert s)
  | (true, _) => ignore 0;

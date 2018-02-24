signature ERRORMSG =
sig
    val anyErrors : bool ref
    val fileName : string ref
    val lineNum : int ref
    val linePos : int list ref
    val sourceStream : TextIO.instream ref
    val error : int -> string -> unit
    exception Error
    val impossible : string -> 'a   (* raises Error *)
    val reset : unit -> unit
end

structure ErrorMsg : ERRORMSG =
struct

  val anyErrors = ref false
  val fileName = ref ""
  val lineNum = ref 1
  val linePos = ref [1]
  val sourceStream = ref TextIO.stdIn

  fun reset() = ( anyErrors:=false
		          ; fileName:=""
		          ; lineNum:=1
		          ; linePos:=[1]
		          ; sourceStream:=TextIO.stdIn
                )

  exception Error

  fun printError s = TextIO.output (TextIO.stdErr, s)

  fun error pos (msg:string) =
      let fun look(a::rest,n) = if a<=pos
                                then ( printError ":"
			                            ; printError (Int.toString n)
			                            ; printError "."
			                            ; printError (Int.toString (pos))
                                     )
	                             else look(rest,n-1)
	         | look _ = printError ":0.0"
      in ( anyErrors := true
	      ; printError (!fileName)
	      ; look (!linePos, !lineNum)
	      ; printError ":"
	      ; printError msg
	      ; printError "\n"
	      ; TextIO.flushOut TextIO.stdErr
         )
      end

  fun impossible msg = ( printError "Error: Compiler bug: "
                       ; printError msg
                       ; printError "\n"
                       ; TextIO.flushOut TextIO.stdErr
                       ; raise Error
                       )

end  (* structure ErrorMsg *)
  

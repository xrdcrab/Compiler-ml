signature ERRORMSG =
sig
    val anyErrors : bool ref
    val fileName : string ref
    val lineNum : int ref
    val linePos : int list ref
    val sourceStream : TextIO.instream ref
    val error : int -> string -> unit
    val getLinePos : int -> string
    exception Error
    val message : string -> unit
    val warn : string -> unit
    val warnAt : int -> string -> unit
    val crash : string -> 'a (* raises Error *)
    val crashAt : int -> string -> 'a
    val impossible : string -> 'a   (* raises Error *)
    val impossibleAt : int -> string -> 'a
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

  fun getLinePos pos = let fun look (a::rest, n) = if a<pos then (Int.toString n) ^ "." ^ (Int.toString (pos-a))
						   else look(rest,n-1)
			     | look _ = "0.0"
		       in look (!linePos, !lineNum)
		       end

  fun position pos = ( printError (!fileName)
		     ; printError "@"
		     ; printError (getLinePos pos)
		     ; printError ": ")
			 
  fun message msg = ( printError msg
		    ; printError "\n"
		    ; TextIO.flushOut TextIO.stdErr )
					 

  fun error pos (msg:string) = ( position pos
			       ; message msg
			       ; anyErrors := true
			       )

  fun warn msg = message (msg ^ "--warning")

  fun warnAt pos msg = ( position pos
		       ; warn msg )
		     
  fun crash msg = ( message (msg ^ "--CRASH")
                  ; raise Error
                  )

  fun crashAt pos msg = ( position pos
			; crash msg )
			    
  fun impossible msg = crash ("compiler bug!" ^ msg)

  fun impossibleAt pos msg = ( position pos
			     ; impossible msg )
			     
end  (* structure ErrorMsg *)
  

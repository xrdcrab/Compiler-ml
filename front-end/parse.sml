structure Parse : sig val parse : string -> Absyn.exp
		                val main : string * string list -> OS.Process.status end =
struct 
  structure TigerLrVals = TigerLrValsFun(structure Token = LrParser.Token)

  structure TigerLex = TigerLexFun(structure Tokens = TigerLrVals.Tokens)

  structure TigerParser = Join(structure ParserData = TigerLrVals.ParserData
                               structure Lex = TigerLex
                               structure LrParser = LrParser)


  fun parse filename =
    let fun parseError(s,p1,p2) = ErrorMsg.error p1 s
    in ( ErrorMsg.reset
       ; ErrorMsg.fileName := filename
       ; let val file = TextIO.openIn filename
             val lexer = TigerLex.makeLexer (fn _ => TextIO.input file)
             val tokens = LrParser.Stream.streamify lexer
             val (absyn, _) = TigerParser.parse(30, tokens, parseError, ())
         in TextIO.closeIn file
          ; absyn
         end
       )
    end handle LrParser.ParseError => Absyn.NilExp

  fun process(p, fs) = let fun pr f = ( TextIO.output (TextIO.stdErr, f)
				                          ; TextIO.output (TextIO.stdErr, "\n")
				                          ; (p f)
                                      ; if !ErrorMsg.anyErrors
                                        then TextIO.output (TextIO.stdErr, "parse failed, continuing...\n")
                                        else ()
				                          ; OS.Process.success )
		                 in let fun loop [] = OS.Process.success
				                    | loop [f] = pr f
				                    | loop (f::fs) = ( pr f
						                               ; loop fs
                                                 )
			                 in loop fs
			                 end
		                 end;

  fun main (_, ("-a" ::  fs)) = process (fn f =>   PrintAbsyn.print (TextIO.stdOut, (parse f)), fs)
    | main (_, ("-u" ::  fs)) = process (fn f => UnparseAbsyn.print (TextIO.stdOut, (parse f)), fs)
    | main (_, (c    ::  fs)) = ( print ("unrecognized command: " ^ c ^ "\n")
				                    ; print "usage: -atu <filenames>\n"
                                ; print "       -a   print data structures\n"
                                ; print "       -u   print unparsed syntax\n"
				                    ; OS.Process.failure
                                )
    | main (_, _) = ( print "usage: -au <filenames>\n"
		              ; OS.Process.failure
                    )
end

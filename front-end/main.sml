structure Main : sig val main : string * string list -> OS.Process.status end =
struct 
  structure TigerLrVals = TigerLrValsFun(structure Token = LrParser.Token)

  structure TigerLex = TigerLexFun(structure Tokens = TigerLrVals.Tokens)

  structure TigerParser = Join(structure ParserData = TigerLrVals.ParserData
                               structure Lex = TigerLex
			       structure LrParser = LrParser)

  fun fileFrontEnd filename file _ = ( ErrorMsg.reset()
                                     ; ErrorMsg.fileName := filename
                                     ; TextIO.output(TextIO.stdErr, filename ^ "\n")
                                     ; let fun get _ = TextIO.input file
	                                        val lexer = LrParser.Stream.streamify (TigerLex.makeLexer get)
                                           fun errProc (s, p1, _) = ( ErrorMsg.error p1 s
                                                                    ; raise ErrorMsg.Error
                                                                    )
	                                        val (absyn, _) = TigerParser.parse(30, lexer, errProc, ())
                                       in absyn
                                       end
                                     )

  fun nameFrontEnd filename _ = let val file = (TextIO.openIn filename)
                                in fileFrontEnd filename file ()
                                   handle ErrorMsg.Error => ( TextIO.closeIn file
                                                            ; raise ErrorMsg.Error
                                                            )
                                end

  fun main (_, args) = let val ME = ref (fn ast => ()) (* a simple middle-end *)
                           fun setME me = ME := (fn ast => me (TextIO.stdOut, ast))
                           val argopts = [ { short = "a"
                                           , long = ["ast"]
                                           , desc = GetOpt.NoArg (fn _ => setME PrintAbsyn.print)
                                           , help = "print AST constructors"
                                           }
                                         , { short = "u"
                                           , long = ["unparse"]
                                           , desc = GetOpt.NoArg (fn _ => setME UnparseAbsyn.print)
                                           , help = "unparse to concrete syntax"
                                           }
                                         ]
                           val (_, fs) = GetOpt.getOpt { argOrder = GetOpt.RequireOrder
                                                       , options = argopts
                                                       , errFn = TextIO.print
                                                       }
                                                       args
                           fun callME fe = !ME (fe ())
                                           handle ErrorMsg.Error => TextIO.output (TextIO.stdErr,
                                                                                   "parse failed, continuing...\n")
                           fun each fs = case fs
                                          of []    => callME (fileFrontEnd "<stdin>" TextIO.stdIn)
                                           | [f]   => callME (nameFrontEnd f)
                                           | f::fs => ( callME (nameFrontEnd f)
                                                      ; each fs
                                                      )

                       in each fs
                        ; OS.Process.success
                       end

end


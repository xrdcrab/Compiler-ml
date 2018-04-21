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

  fun main (_, args) = let val ME = ref (fn ast => ())       (* a simple middle-end *)
                           fun liftME me = ME := (fn ast => me (TextIO.stdOut, ast))

                           val BE = ref (fn frag => ())      (* a simple back-end *)
                           fun liftBE be = BE := (fn frag => be (TextIO.stdOut, RiscVFrame.mainFrame, frag))

                           val MO = ref (fn stm => [stm])    (* a simple middle-end optimizer *)
                           fun setMO mo = MO := mo

                           val BO = ref (fn (ins, frm) => ins) (* a simple back-end optimizer *)
                           fun setBO bo = BO := bo
                                          
                           fun printFrag pPROC pSTR (str, frm, frag) = case frag
                                                                        of Translate.PROC {body, frame={name,formals,sp}} =>
                                                                                                         ( TextIO.output (str, (";PROC " ^ (Symbol.name name) ^ ":\n"))
                                                                                                                                            ; pPROC (str, frm, ((!MO) body))
                                                                           )
                                                                         | Translate.STRING (l, s) => pSTR (str, l, s)
                                                                                                      
                           fun printString (str, l, s) =  TextIO.output(str, ("STRING " ^ (Symbol.name l) ^ ":\"" ^ s ^ "\"\n"))
                                                          
                           fun liftPrintStm printer (str, _, stms) = app (fn stm => printer (str, stm))
                                                                         stms
                           fun withFrag f (str, frag) = case frag
                                                         of Translate.PROC {body, frame={name,formals,sp}} =>
						            ( TextIO.output (str, ("PROC " ^ (Symbol.name name)
                                                                                   ^ ":\n"))
							    ; app (fn stm => f (str, stm))
                                                                  ((!MO) body)
                                                            )
                                                          | Translate.STRING (l, s) =>
                                                            TextIO.output(str, ("STRING " ^ (Symbol.name l)
										^ ":\""
										^ (String.toString s) ^ "\"\n"))
									 
									 
                           val argopts = [ { short = "p"
                                           , long = ["print"]
                                           , desc = GetOpt.NoArg (fn _ => ( liftME PrintAbsyn.print
                                                                          ; liftBE (printFrag (liftPrintStm PrintTree.print)
											      printString)
								          ))
                                           , help = "print with constructors"
                                           }
                                               
                                         , { short = "u"
                                           , long = ["unparse"]
                                           , desc = GetOpt.NoArg (fn _ => ( liftME UnparseAbsyn.print
                                                                          ; liftBE (printFrag (liftPrintStm UnparseTree.print)
									                      printString)
									  ))
                                           , help = "unparse to concrete syntax"
                                           }
					       
                                         , { short = "t"
                                           , long = ["trace"]
                                           , desc = GetOpt.NoArg (fn _ => setMO (fn stm => Canon.traceSchedule
                                                                                               (Canon.basicBlocks
                                                                                                    (Canon.linearize stm))))
                                           , help = "interpolate trace scheduling"
                                           }

                                         , { short = "c"
                                           , long = ["check"]
                                           , desc = GetOpt.NoArg (fn _ => ME := (fn ast =>
                                                                                   (app (!BE) (Semant.transProg ast)
                                                                                    handle ErrorMsg.Error => TextIO.output (TextIO.stdErr,
                                                                                                                            "checking failed, continuing ...\n"))))
                                           , help = "add type-checking and IR translation"
                                           }
                                                                                           
                                         , { short = "e"
                                           , long = ["escape"]
                                           , desc = GetOpt.NoArg (fn _ => let val oME = !ME
                                                                          in ME := (fn ast => ( FindEscape.findEscape ast
											      ; oME ast ))
                                                                          end)
                                           , help = "interpolate escape analysis"
                                           }

					 , { short = "x"
					   , long = ["execute"]
					   , desc = GetOpt.NoArg (fn _ => liftME Interp.exec)
					   , help = "interpret program"
					   }

                                         , { short = "a"
                                           , long = ["generate assembly"]
                                           , desc = GetOpt.NoArg (fn _ => liftBE (printFrag (fn (str, frm, stms) => app (fn i => TextIO.output(str, Assem.format Temp.makestring i))
                                                                                                                        ((!BO) (List.concat (map (RiscVGen.codegen frm)
                                                                                                                                                 stms), frm)))
                                                                                            (fn (str, l, s) => TextIO.output (str, RiscVFrame.string (l, s)))))
                                             
                                           , help = "print assembly"
                                           }

                                         , { short = "r"
                                           , long = ["register allocate"]
                                           , desc = GetOpt.NoArg (fn _ => liftBE (printFrag (fn (str, frm, stms) => let val insts = ((!BO) (List.concat (map (RiscVGen.codegen frm)
                                                                                                                                                             stms), frm))
                                                                                                                        val (insts, alloc) = RegAlloc.alloc(insts, frm)
                                                                                                                        fun tempPrinter t = case Temp.Table.look(alloc, t)
                                                                                                                                              of NONE => Temp.makestring t
                                                                                                                                               | SOME x => "$" ^ x
                                                                                                                    in app (fn i => TextIO.output(str, Assem.format tempPrinter i))
                                                                                                                           insts
                                                                                                                    end)
                                                                                            (fn (str, l, s) => TextIO.output (str, RiscVFrame.string (l, s)))))
                                           , help = "allocate registers"
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

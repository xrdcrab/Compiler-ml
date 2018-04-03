signature TRANSLATE =
sig
    type level
    type access			(* not the same as FRAME.access *)

    datatype exp = Ex of Tree.exp
		           | Nx of Tree.stm
		           | Cx of Temp.label * Temp.label -> Tree.stm

    datatype frag = PROC of { body : Tree.stm
                            , frame : RiscVFrame.frame
                            }
		            | STRING of Temp.label * string

    val outermost : level
    val newLevel : { parent : level
                   , name : Temp.label
                   , formals : bool list
                   } -> level
    val formals : level -> access list
    val allocLocal : level -> bool -> access

    val initResult : unit -> unit
    val getResult : unit -> frag list

    val unEx : exp -> Tree.exp
    val unNx : exp -> Tree.stm
    val unCx : exp -> (Temp.label * Temp.label) -> Tree.stm

    val xlNil : exp
    val xlInt : int -> exp
    val xlBool : bool -> exp
    val xlString : string -> exp
    val xlIntComp : Absyn.oper * exp * exp -> exp
    val xlBoolComp : Absyn.oper * exp * exp -> exp
    val xlStrComp : Absyn.oper * exp * exp -> exp
    val invert : exp -> exp
    val xlPtrEq : exp * exp -> exp
    val xlArith : Absyn.oper * exp * exp -> exp
    val xlIfThen : exp * exp -> exp
    val xlIfThenElse : exp * exp * exp -> exp
    val xlWhile : exp * exp * Temp.label -> exp
    val xlBreak : Temp.label option -> exp
    val xlSeq : exp * exp -> exp
    val xlAssign : exp * exp -> exp
    val xlArray : exp * exp -> exp
    val xlRecord : (exp * int) list -> exp
    val xlCall : (level * level * Temp.label * exp list) -> exp
    val xlLet : exp list * exp -> exp
    val xlSimpleVar : access * level -> exp
    val xlSubscript : exp * exp -> exp
    val xlField : exp * int -> exp
			       
    val xlVarDec : access * level * exp -> exp
    val xlUnitFunDec : level * exp -> unit (* aka procEntryExit *)
    val xlFunDec : level * exp -> unit
end

structure Translate : TRANSLATE =
struct
  structure A = Absyn
  structure E = ErrorMsg
  structure F = RiscVFrame
  structure M = Temp
  structure R = Tree

  datatype level = Outermost of F.frame
		 | FunLevel of { frame: F.frame
                               , parent: level
                               , unique : unit ref
                               }

  type access = level * F.access

  datatype frag = PROC of { body : Tree.stm
                          , frame : F.frame
                          }
		| STRING of Temp.label * string

  fun newLevel {parent, name, formals} = FunLevel { frame = F.newFrame { name = name
								       , formals = true::formals
                                                                       }
                                                  , parent = parent
						  , unique = ref ()
                                                  }
					          
  val outermost = Outermost F.mainFrame
                  
  fun formals (Outermost _) = []
    | formals (level as FunLevel{frame,parent,unique}) = map (fn a => (level, a)) (F.formals frame)

  fun allocLocal (level as Outermost frame)               escape = (level, F.allocLocal frame escape)
    | allocLocal (level as FunLevel{frame,parent,unique}) escape = (level, F.allocLocal frame escape)

  datatype exp = Ex of R.exp
	            | Nx of R.stm
	            | Cx of M.label * M.label -> R.stm

  fun seq [] = R.EXP (R.CONST 0)
    | seq [s] = s
    | seq (s::ss) = R.SEQ (s, seq ss)

  fun jump l = R.JUMP (R.NAME l, [l])

  val ZERO = R.CONST 0
             
  val ONE = R.CONST 1

  fun unEx (Ex e) = e
    | unEx (Cx genstm) = let val r = M.newtemp ()
			                    val t = M.newlabel ()
			                    val f = M.newlabel ()
			                in R.ESEQ (seq[           R.MOVE (R.TEMP r, ONE),
						                                 genstm (t, f),
					                         R.LABEL f, R.MOVE (R.TEMP r, ZERO),
					                         R.LABEL t                          ],
				                            R.TEMP r)
			                end
    | unEx (Nx s) = R.ESEQ (s, ZERO)


  fun unNx (Ex (R.ESEQ (s, e))) = R.SEQ (s, R.EXP e)
    | unNx (Ex e) = R.EXP e
    | unNx (Nx s) = s
    | unNx (Cx genstm) = let val l = M.newlabel ()
			                in R.SEQ(genstm (l, l),
				                      R.LABEL l)
			                end

  fun unCx (Ex (R.CONST 0)) = (fn (_, f) => jump f)
    | unCx (Ex (R.CONST 1)) = (fn (t, _) => jump t)
    | unCx (Ex e) = (fn (t, f) => R.CJUMP (R.EQ, e, ZERO, f, t))
    | unCx (Nx s) = E.impossible "STMT TO COND"
    | unCx (Cx genstm) = genstm

  fun invert (Ex e) = let val t = M.newlabel ()
			                 val f = M.newlabel ()
			                 val ret = M.newtemp ()
		                in Ex (R.ESEQ (seq [           R.MOVE (R.TEMP ret, ZERO),
					                                      R.CJUMP (R.EQ, e, ZERO, t, f),
	                                       R.LABEL t, R.MOVE (R.TEMP ret, ONE),
					                           R.LABEL f ],
				                         R.TEMP ret))
		     end
    | invert (Cx test) = Cx (fn (t, f) => test (f, t))
    | invert (Nx s) = Ex (R.ESEQ (s, ONE))
		      
  fun incr a = R.MOVE (a, R.BINOP (R.PLUS, a, ONE))
  fun incrWS a = R.MOVE (a, R.BINOP (R.PLUS, a, R.CONST F.wordSize))

  (* fragments *)
  val frags : frag list ref = ref []
			      
  fun addFrag f = frags := f :: !frags

  fun initResult () = frags := []

  fun getResult () = !frags

  (* translations *)
  fun xlInt i = Ex (R.CONST i)

  fun xlBool b = Ex (if b then ONE else ZERO)

  val xlNil = Ex (ZERO)

  fun xlString s = let val label = M.newlabel ()
		             in addFrag (STRING (label, s))
		                ; Ex (R.NAME label)
                   end

  fun aop2relop A.EqOp     = R.EQ
    | aop2relop A.NeqOp    = R.NE
    | aop2relop A.LtOp     = R.LT
    | aop2relop A.LeOp     = R.LE
    | aop2relop A.GtOp     = R.GT
    | aop2relop A.GeOp     = R.GE
    | aop2relop _          = E.impossible "RELOP INCONSISTENCY"

  fun aop2binop A.PlusOp   = R.PLUS
    | aop2binop A.MinusOp  = R.MINUS
    | aop2binop A.TimesOp  = R.MUL
    | aop2binop A.DivideOp = R.DIV
    | aop2binop _          = E.impossible "BINOP INCONSISTENCY"

  fun xlStrComp (ao, l, r) = Cx (fn (t, f) => R.CJUMP ( aop2relop ao
                                                      , R.CALL ( R.NAME (M.namedlabel ("stringCompare"))
                                                               , map unEx [l, r])
                                                      , ZERO
                                                      , t
                                                      , f))

  fun xlArith (ao, l, r) = Ex (R.BINOP (aop2binop ao,  unEx l, unEx r))

  fun xlIntComp (ao,  l, r) = Cx (fn (t, f) => R.CJUMP (aop2relop ao, unEx l, unEx r, t, f))

  val xlBoolComp = xlIntComp

  fun xlPtrEq (l, r) = Ex (R.CALL (R.NAME (M.namedlabel ("ptrEqual")), map unEx [l, r]))

  fun xlIfThenElse (test, Nx then', Nx else') = let val t = M.newlabel ()
			                                           val f = M.newlabel ()
			                                           val e = M.newlabel ()
		                                          in Nx (seq [           (unCx test) (t, f),
 	                                                         R.LABEL t, then',
								                                               jump e,
					                                             R.LABEL f, else',
					                                             R.LABEL e                    ])
		end
    | xlIfThenElse (test, then', else') = let val t = M.newlabel ()
					                               val f = M.newlabel ()
					                               val e = M.newlabel ()
					                               val r = M.newtemp ()
					                           in Ex (R.ESEQ ( seq [           (unCx test) (t, f),
							                                          R.LABEL t, R.MOVE (R.TEMP r, (unEx then')),
								 	                                               jump e,
							                                          R.LABEL f, R.MOVE (R.TEMP r, (unEx else')),
							                                          R.LABEL e  ]
                                                       , R.TEMP r))
					  end

  fun xlIfThen (test, then' as Ex _) = xlIfThenElse (test, then', Ex (ZERO))

    | xlIfThen (test, Cx then') = let val z = M.newlabel ()
				                      in Cx (fn (t,f) => seq [           (unCx test) (z, f),
							                                     R.LABEL z, then' (t, f)      ])
				  end
    | xlIfThen (test, Nx then') = let val t = M.newlabel ()
				                          val f = M.newlabel ()
				                      in Nx (seq [           (unCx test) (t, f),
					                               R.LABEL t, then',
				                                  R.LABEL f                    ])
				  end

  fun xlWhile (test, body, break) = let val top = M.newlabel ()
					                         val inner = M.newlabel ()
				                        in Nx (seq [R.LABEL top,   (unCx test) (inner, break),
						                              R.LABEL inner, (unNx body),
	                                                            jump top,
						                              R.LABEL break ])
				                        end

  fun xlBreak NONE = E.impossible "BREAK INCONSISTENCY"
    | xlBreak (SOME break) = Nx (jump break)

  fun xlSeq (exps, exp) = Ex (R.ESEQ (unNx exps, unEx exp))
                          
  fun xlAssign (expv, expe) = Nx (R.MOVE (unEx expv, unEx expe))

  fun xlArray (expn, expi) = let val arr = M.newtemp ()
				                     val ptr = M.newtemp ()
				                     val cnt = M.newtemp ()
				                     val siz = M.newtemp ()
				                     val ini = M.newtemp ()
				                     val test = M.newlabel ()
				                     val loop = M.newlabel ()
				                     val done = M.newlabel ()
			                    in Ex (R.ESEQ (seq [               R.MOVE (R.TEMP siz, unEx expn),
						                                              R.MOVE (R.TEMP arr, R.CALL (R.NAME (M.namedlabel "allocSpace"), [R.TEMP siz])),
								                                        R.MOVE (R.TEMP arr, R.TEMP ptr),
						                                              R.MOVE (R.MEM (R.TEMP ptr), R.TEMP siz), (* put arraysize in position 0 *)
								                                        incrWS (R.TEMP ptr),
						                                              R.MOVE (R.TEMP ini, unEx expi),
						                                              R.MOVE (R.TEMP cnt, ZERO),
						                                              R.LABEL test, R.CJUMP (R.GE, R.TEMP cnt, R.TEMP siz, done, loop),
						                                R.LABEL loop, R.MOVE (R.MEM (R.TEMP ptr), R.TEMP ini),
								                                        incrWS (R.TEMP ptr),
								                                        incr (R.TEMP cnt),
								                                        jump test,
						                                R.LABEL done ]
					                           , R.TEMP arr))
			     end

  (* NB: populate fields in programmer order, to preserve side-effects *)
  fun xlRecord ens = let val rcd = M.newtemp ()
			                val ptr = M.newtemp ()
		               in Ex (R.ESEQ ( foldl (fn (s, s') => R.SEQ (s', s))
					                            (R.MOVE (R.TEMP rcd, R.CALL ( R.NAME (M.namedlabel "allocSpace")
                                                                       , [R.CONST (length ens)])))
					                            (map (fn (exp, fnum) => R.SEQ (R.MOVE (R.TEMP ptr,
                                                                                  R.BINOP (R.PLUS, R.TEMP rcd,
                                                                                           R.CONST (fnum * F.wordSize))),
									                                               R.MOVE (R.MEM (R.TEMP ptr), unEx exp)))
						                              ens)
				                       , R.TEMP rcd))
		     end

  (* definition_level * call_level -> exp *)
  fun walkStatic (Outermost _,                                    Outermost _) = R.TEMP F.FP
    | walkStatic (_,                                              Outermost _) = E.impossible "STATIC LINKAGE INCONSISTENCY"
    | walkStatic (level' as Outermost _,                          FunLevel {frame,parent,unique}) = F.exp (F.staticLink frame) (walkStatic (level', parent))
    | walkStatic (level' as FunLevel {frame=_,parent=_,unique=u}, FunLevel {frame,parent,unique=u'}) =
      if u=u'
		then R.TEMP F.FP
		else F.exp (F.staticLink frame) (walkStatic (level', parent))

  fun xlCall (level, Outermost _, label, exps) = Ex (R.CALL (R.NAME label, map unEx exps))
    | xlCall (level, FunLevel {frame,parent,unique}, label, exps) =
      Ex (R.CALL (R.NAME label, ((walkStatic (parent, level)) :: (map unEx exps))))
					    
  fun xlLet ([], body) = body
    | xlLet (inits, body) = Ex (R.ESEQ (seq (map unNx inits), unEx body))
			    
  (* yields an R.MEM or an R.TEMP expression for use as an l-value or an r-value *)
  fun xlSimpleVar ((level', access), level) = Ex (F.exp access (walkStatic (level', level)))

  (* array is [length, item 0, item 1, ... ] -- yields an R.MEM expression for use as an l-value or an r-value *)
  fun xlSubscript (expv, expi) = let val a = M.newtemp ()
				                         val i = M.newtemp ()
				                         val t = M.newlabel ()
				                         val f = M.newlabel ()
				                     in Ex (R.ESEQ (seq [           R.MOVE (R.TEMP a, unEx expv),
								                                        R.MOVE (R.TEMP i, unEx expi),
								                                        R.CJUMP (R.LE, R.TEMP i, R.MEM (R.TEMP a), t, f),
						                                   R.LABEL f, R.EXP (R.CALL ( R.NAME (M.namedlabel "arrayOutOfBounds")
                                                                              , [])), (* never to return *)
						                                   R.LABEL t                                                          ]
						                            , R.MEM(R.BINOP (R.PLUS, R.TEMP a,
                                                                       R.BINOP (R.MUL,
                                                                                R.BINOP (R.PLUS, R.TEMP i, ONE),
                                                                                R.CONST (F.wordSize))))))
				                     end

  (* record is [field 0, field 1, ...] -- yields an R.MEM expression for use as an l-value or an r-value *)
  fun xlField (expv, n) = let val r = M.newtemp ()
			                     val t = M.newlabel ()
			                     val f = M.newlabel ()
			                 in Ex (R.ESEQ ( seq [           R.MOVE (R.TEMP r, unEx expv),
							                                     R.CJUMP (R.NE, R.TEMP r, ZERO, t, f),
	                                            R.LABEL f, R.EXP (R.CALL ( R.NAME (M.namedlabel "nilRecordPtr")
                                                                        , [])), (* never to return *)
					                                R.LABEL t                                                       ]
					                         , R.MEM (R.BINOP (R.PLUS, R.TEMP r,
                                                                 R.BINOP (R.MUL, R.CONST n,
                                                                                 R.CONST (F.wordSize))))))
			                 end


  fun xlVarDec (access, level, expi) = xlAssign (xlSimpleVar (access, level), expi)

  fun registerProc (frame, stm) = addFrag (PROC { body = F.procEntryExit1 (frame, stm)
						                              , frame = frame
                                                })

  fun getFrame (Outermost frame) = frame
    | getFrame (FunLevel {frame, parent, unique}) = frame

  fun xlUnitFunDec (level, expb) = registerProc (getFrame level, unNx expb)
							    
  fun xlFunDec (level, expb) = registerProc (getFrame level, R.MOVE (R.TEMP F.RV, unEx expb))
end

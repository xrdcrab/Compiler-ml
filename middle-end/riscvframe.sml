structure RiscVFrame : FRAME =
struct 
  structure A = Assem
  structure E = ErrorMsg
  structure M = Temp
  structure R = Tree
  structure S = Symbol

  type register = string

  (* fix these ... they are clearly insufficient *)
  val wordSize = 0
  val FP = M.newtemp ()
  val FPreg = ("fp", FP)
		  
  val SP = M.newtemp ()
  val SPreg = ("sp", SP)

  val LR = M.newtemp ()
  val LRreg = ("ra", LR)

  (* other registers not mentioned:
   *     pc -- program counter
   *
   *     zero -- always zero    (x0)
   *     gp   -- global pointer (x3)
   *     tp   -- thread pointer (x4)
   *)

  fun makeReg (s, n) = (s ^ (Int.toString n), Temp.newtemp ())
			   
  fun makeRegs (s, n1, n2) = if (n1=n2)
                             then [makeReg (s, n1)]
			     else (makeReg (s, n1))::makeRegs (s, n1+1, n2)
                                    
  (* special registers *)
  val spregs = [ FPreg, SPreg, LRreg ]
  val specialregs = map (fn (s, _) => s) spregs

  (* return value registers *)
  val RV = Temp.newtemp ()    (* guaranteed one return value register *)
  val RVreg = ("a0", RV)
  val rregs = [ RVreg, ("a1", Temp.newtemp()) ] (* name must be same as argument register *)
  val rvregs = map (fn (s, _) => s) rregs

  (* argument registers *)
  val aregs = makeRegs ("a", 2, 7) @ rregs
  val argregs = map (fn (s, _) => s) aregs

  (* variable registers *)
  val eregs = makeRegs ("s", 1, 11) (* callee save ... note s0 is fp so exclude it *)
  val tregs = makeRegs ("t", 0, 6)
  val vregs = aregs @ eregs @ tregs
  val varregs = map (fn (s, _) => s) vregs

  val registers = specialregs @ varregs (* note that rvregs \in argregs \in varregs *)

  val tempMap = foldr (fn ((s, m), t) => M.Table.enter (t, m, s))
		      M.Table.empty
		      ( spregs (* @ ??? *)) (* fill this in *)
		  
  fun temp2string t = case (M.Table.look (tempMap, t))
		       of NONE => M.makestring t
			| SOME s => s



  fun externalCall (_, _) = R.CONST 0 (* place holder ... fill this in *)

  datatype access = InFrame of int
                  | InReg of Temp.temp

  (*
   *          CALLER's FRAME
   *      8 arguments passed in registers, remaining ones on stack above the sp
   *                                       because they belong to the caller
   *    FP -------------------------------
   *	    RA save area
   *        FP save area
   *        other registers ...
   *        arguments to functions we call
   *    SP -------------------------------
   *          FRAME for functions we call
   *
   *    Typical prologue and epilogue
   *              entry_label:                  # PROLOGUE
   *                 sw   ra, 0(sp)
   *                 sw   fp, -4(sp)
   *                 mv   fp, sp
   *                 addi sp, sp, -framesize
   *                    # .... body of function ....
   *                 mv   sp, fp                # EPILOGUE
   *                 lw   fp, -4(sp)
   *                 lw   ra,  0(sp)
   *                 ret
   *)
				 
  type frame = { name : M.label
               , formals : access list
               , sp : int ref
               }

  val wordSize = 4 (* bytes *)

		         fun newFrame {name, formals} = let val maxIncomingRegs = 8
				                           val firstIncomingOffset = wordSize
				                           fun placeFormal (_, (n, 0, formals)) = (n+wordSize, 0, (InFrame n)::formals)
					                          | placeFormal (true, (n, r, formals)) = (n+wordSize, r, (InFrame n)::formals)
					                          | placeFormal (false, (n, r, formals)) = (n, r-1, (InReg (M.newtemp())::formals))
				                       in let val (_, _, formals) = foldl placeFormal (firstIncomingOffset, maxIncomingRegs, []) formals
				                          in {name = name, formals = formals, sp = ref 0}
				                          end
				                       end

    fun formals {name, formals=formals, sp} = formals

    fun name {name=name, formals, sp} = name
			
    fun postdec r = let val n = !r
		    in ( r := n - wordSize
		       ; n )
		    end

    fun allocLocal {name, formals, sp=sp} true = InFrame (postdec sp)
      | allocLocal _ false = InReg (M.newtemp ())
			     
    fun externalCall (s, args) = R.CALL (R.NAME (Temp.namedlabel ("_" ^ s)), args)

    (* yields an R.MEM or R.TEMP which can be used as an l-value or an r-value *)
    fun exp (InFrame 0) fp = R.MEM fp
      | exp (InFrame k) fp = if k<0 then R.MEM (R.BINOP (R.MINUS, fp, (R.CONST (0-k))))
			                    else R.MEM (R.BINOP (R.PLUS, fp, (R.CONST k)))
      | exp (InReg r) _ = R.TEMP r
                          
    fun staticLink {name,formals=[],sp} = E.impossible "NO STATIC LINK"
      | staticLink {name,formals=(sl::realFormals),sp} = sl


    fun procEntryExit1 (frame, stm) = stm (* dummy as on p.170 *)
                                      
    fun procEntryExit2 (frame, body) = body
                                       @ [] (* fill this in--cf. pp. 208--209 *)
                                       
    fun procEntryExit3 ({name, formals, sp}, body) = { prolog = "PROCEDURE " ^ S.name name ^ "\n"  (* dummy on p.209 *)
	                                             , body=body
	                                             , epilog = "END " ^ S.name name ^ "\n"
                                                     }

    fun string (l, s) = (S.name l) ^ ": .asciiz \"" ^ (String.toString s) ^ "\"\n"

    val mainFrame = newFrame { name = M.namedlabel "tigermain"
			     , formals = []
                             }

    datatype frag = PROC of { body : Tree.stm
                            , frame : frame }
                  | STRING of Temp.label * string

end

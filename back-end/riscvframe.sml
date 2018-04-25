structure RiscVFrame : FRAME =
struct 
  structure A = Assem
  structure E = ErrorMsg
  structure M = Temp
  structure R = Tree
  structure S = Symbol

  type register = string

  (* fix these ... they are clearly insufficient *)
  val wordSize = 4
  val FP = M.newtemp ()
  val FPreg = ("fp", FP)
      
  val SP = M.newtemp ()
  val SPreg = ("sp", SP)

  val LR = M.newtemp ()
  val RAreg = ("ra", LR)

  val ZERO = M.newtemp ()
  val RA = M.newtemp ()
  val formalRegNum = 6    (* a2 - a7 *) 
  val rvRegNum = 2        (* a0 a1 *)
  val tempRegNum = 7      (* t0 - t6 *)
  val callerSaveNum = 15  
  val calleeSaveNum = 11  (* note s0 is fp so exclude it *)

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
  val spregs = [ FPreg, SPreg, RAreg]
  val specialregs = map (fn (s, _) => s) spregs
  val speRegs = map (fn (_,t) => t) spregs

  (* return value registers *)
  val RV = Temp.newtemp ()    (* guaranteed one return value register *)
  val RVreg = ("a0", RV)
  val rregs = [ RVreg, ("a1", Temp.newtemp()) ] (* name must be same as argument register *)
  val rvregs = map (fn (s, _) => s) rregs

  (* argument registers. a0 a1 are return value registers *)
  val aregs = makeRegs ("a", 2, 7) @ rregs
  val argregs = map (fn (s, _) => s) aregs

  (* variable registers *)
  val eregs = makeRegs ("s", 1, 11) (* callee save ... note s0 is fp so exclude it *)
  val eeregs = map (fn (s, _) => s) eregs
  
  val tregs = makeRegs ("t", 0, 6)  (* temp registers *)
  val ttregs = map (fn (s, _) => s) tregs

  val regs = spregs @ eregs  @ tregs@ aregs 

  val registers = map (fn (s, _) => s) regs
  val registerTable = map (fn (_,t) => t) regs
  val calleeRegs = map (fn (_,t) => t) eregs


  val tempMap = foldr (fn ((s, m), t) => M.Table.enter (t, m, s))
          M.Table.empty
          (ListPair.zip(registers,registerTable)) (* fill this in *)
      
  fun temp2string t = case (M.Table.look (tempMap, t))
           of NONE => M.makestring t
      | SOME s => s



  fun externalCall (functionName, args) = R.CALL(R.NAME(M.namedlabel(functionName)), args) (* place holder ... fill this in *)

  datatype access = InFrame of int
                  | InReg of Temp.temp

  (*
   *          CALLER's FRAME
   *      8 arguments passed in registers, remaining ones on stack above the sp
   *                                       because they belong to the caller
   *    FP -------------------------------
   *      RA save area
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
  
  (* val formals : frame -> access list *)
  fun formals {name, formals=formals, sp} = formals
  
  (* val name : frame -> Temp.label *)
  fun name {name=name, formals, sp} = name
      
  fun postdec r = let val n = !r
      in ( r := n - wordSize
         ; n )
      end
  
  (* val allocLocal : frame -> bool -> access *)
  fun allocLocal {name, formals, sp=sp} true = InFrame (postdec sp)
    | allocLocal _ false = InReg (M.newtemp ())
         
  fun externalCall (s, args) = R.CALL (R.NAME (Temp.namedlabel ("_" ^ s)), args)

  (* yields an R.MEM or R.TEMP which can be used as an l-value or an r-value *)
  (* val exp : access -> Tree.exp -> Tree.exp *)
  fun exp (InFrame 0) fp = R.MEM fp
    | exp (InFrame k) fp = if k<0 then R.MEM (R.BINOP (R.MINUS, fp, (R.CONST (0-k))))
                        else R.MEM (R.BINOP (R.PLUS, fp, (R.CONST k)))
    | exp (InReg r) _ = R.TEMP r
    
  (* val staticLink : frame -> access *)                      
  fun staticLink {name,formals=[],sp} = E.impossible "NO STATIC LINK"
    | staticLink {name,formals=(sl::realFormals),sp} = sl


  fun seq[] = R.EXP(R.CONST 0)
      | seq[exp'] = exp'
      | seq(exp'::exps) = R.SEQ(exp', (seq exps))

  fun restoreReg (register, access) = R.MOVE(R.TEMP(register), (exp access (R.TEMP FP)))

  fun saveReg (register, access) = R.MOVE((exp access (R.TEMP FP)), R.TEMP(register))

  fun procEntryExit1 ({name, formals, sp}, stm) = stm
                                    
  fun procEntryExit2 (frame, body) = body
                                      @ 
                          [A.OPER{assem="",
                                  src=[ZERO,RA,SP]@calleeRegs,
                                  dst=[],jump=SOME[]}]                                     
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

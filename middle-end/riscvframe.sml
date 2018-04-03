structure RiscVFrame : FRAME =
struct 
  structure M = Temp
  structure R = Tree

  type register = string

  (* fix these ... they are clearly insufficient *)
  val wordSize = 4
  val FP = M.newtemp ()
  val SP = M.newtemp ()
  val RV = M.newtemp ()

  (* callee saves : $x9($s1), $x18-27($s2-11), $x2(sp), $x8($s0/fp) *)
  val s1 = M.newtemp()
  val s2 = M.newtemp()
  val s3 = M.newtemp()
  val s4 = M.newtemp()
  val s5 = M.newtemp()
  val s6 = M.newtemp()
  val s7 = M.newtemp()
  val s8 = M.newtemp()
  val s9 = M.newtemp()
  val s10 = M.newtemp()
  val s11 = M.newtemp()
  val s12 = M.newtemp()

  val formalReg1 = M.newtemp()
  val formalReg2 = M.newtemp()
  val formalReg3 = M.newtemp()
  val formalReg4 = M.newtemp()
  val formalReg5 = M.newtemp()
  val formalReg6 = M.newtemp()
  val formalReg7 = M.newtemp()
  val formalReg8 = M.newtemp()

  val formalRegNum = 8
  val rvRegNum = 2
  val calleeSaveNum = 12
  val calleeSaves = [s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12]
  val formalRegs = [formalReg1,formalReg2,formalReg3,formalReg4,formalReg5,formalReg6,formalReg7,formalReg8]
  
  (* val externalCall : string * Tree.exp list -> Tree.exp *)
  fun externalCall (functionName, args) = R.CALL(R.NAME(M.namedlabel(functionName)), args)

  datatype access = InFrame of int        (* mem location at offset int from the frame pointer *)
                  | InReg of Temp.temp    (* in register Temp.temp *)

  type frame = { name : M.label
               , formals : access list
               , sp : int ref
               }
  
  (* val name : frame -> Temp.label *)
  fun name ({name,formals,sp}) = name

  fun generateAccesses ( formal,(formals, sp, count)) =
    if count > 8 then 
      let 
         val sp' = sp - wordSize
      in 
         (InFrame(sp')::formals, count+1, sp')
      end   
     else
      (if formal then 
        let 
           val sp' =  sp - wordSize
        in
           (InFrame(sp')::formals, count+1, sp')
        end
      else
        (InReg(M.newtemp())::formals, count+1, sp))
  
  (* val newFrame : {name : Temp.label, formals : bool list} -> frame *)
  fun newFrame { name, formals } = let 
                                      val (accesses, _,stackPointer) = foldr generateAccesses ([],0,0) formals
                                   in 
                                      {name = name, formals = accesses, sp = ref stackPointer}
                                   end

  (* val formals : frame -> access list *)
  fun formals ({name,formals,sp}) = formals

  (* val allocLocal : frame -> bool -> access *)
  fun allocLocal {name, formals, sp} escape = if escape then
                                                (sp := !sp - wordSize
                                                 ; InFrame(!sp))
                                              else 
                                                InReg(M.newtemp())

  (* val exp : access -> Tree.exp -> Tree.exp *)
  fun exp (InReg t) _ = R.TEMP(t)
    | exp (InFrame offset) framePointer = R.MEM(R.BINOP(R.PLUS,framePointer,R.CONST(offset)))

  (* val staticLink : frame -> access *)   
  fun staticLink ({name,formals,sp}) = InFrame 0

  fun seq[] = R.EXP(R.CONST 0)
      | seq[exp'] = exp'
      | seq(exp'::exps) = R.SEQ(exp', (seq exps))

  fun restoreReg (register, access) = R.MOVE(R.TEMP(register), (exp access (R.TEMP FP)))

  fun saveReg (register, access) = R.MOVE((exp access (R.TEMP FP)), R.TEMP(register))

  (* val procEntryExit1 : frame * Tree.stm -> Tree.stm *)
  fun procEntryExit1 ({name, formals, sp}, body) = let
                                                      val regSaves = RV::calleeSaves
                                                      val tempRegSaves = map (fn(a) => allocLocal {name=name,formals=formals,sp=sp} true) regSaves
                                                      val saveRegToTemp = seq(ListPair.map saveReg (regSaves, tempRegSaves))
                                                      val restoreRegFromTemp = seq(ListPair.map restoreReg (regSaves, tempRegSaves))
                                                      val bodySeq = seq[saveRegToTemp, body, restoreRegFromTemp]
                                                   in
                                                      (case formals
                                                         of [] => bodySeq
                                                          | _  => R.SEQ(seq(ListPair.map (fn (register, access) => R.MOVE(R.TEMP(register), (exp access (R.TEMP FP)))) (formalRegs,formals)),
                                                                        bodySeq))
                                                   end

  datatype frag = PROC of { body : Tree.stm
                          , frame : frame }
                | STRING of Temp.label * string

end

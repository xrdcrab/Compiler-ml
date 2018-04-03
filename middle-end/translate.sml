signature TRANSLATE =
sig
    structure Frame : FRAME

    type level
    type access			(* not the same as FRAME.access *)

    val outermost : level

    val newLevel : { parent : level
                   , name : Temp.label
                   , formals : bool list } -> level

    val formals : level -> access list
    val allocLocal : level -> bool -> access




    datatype exp = Ex of Tree.exp
                 | Nx of Tree.stm
                 | Cx of Temp.label * Temp.label -> Tree.stm


    datatype frag = PROC of { body : Tree.stm
                            , frame : Frame.frame }
                  | STRING of Temp.label * string
    val fragments : frag list ref
    val initResult : unit -> unit
    val getResult : unit -> frag list

    val newBreakPos : unit -> Temp.label
    val seq : Tree.stm list -> Tree.stm
    val unEx : exp -> Tree.exp
    val unNx : exp -> Tree.stm
    val unCx : exp -> (Temp.label * Temp.label -> Tree.stm)
    val opers : exp * Absyn.oper * exp -> exp
    val assign : exp * exp -> exp
    val intExp : int -> exp 
    val fieldVar : exp * Symbol.symbol * Symbol.symbol list -> exp
    val subscriptVar : exp * exp -> exp
    val letExp : exp list * exp -> exp
    val nilExp : exp
    val seqExp : exp list -> exp
    val break : Tree.label -> exp
    val arrayExp : exp * exp -> exp
    val ifExp : exp * exp -> exp
    val ifElse : exp * exp * exp -> exp
    val whileExp : exp * exp * Tree.label -> exp
    val for : exp * Tree.label * exp * exp * exp -> exp
    val record : exp list -> exp
    val error : exp 
    val boolExp : bool -> exp
    val callExp : Temp.label * exp list * level * level -> exp
    val computeSL : level * level * Tree.exp -> Tree.exp
    val checkLevel : level * level -> bool
    val stringOpers : exp * Absyn.oper * exp -> exp
    val simpleVar : access * level -> exp
    val stringExp : string -> exp
end

structure Translate : TRANSLATE =
struct
  structure A = Absyn
  structure E = ErrorMsg
  structure Frame = RiscVFrame
  structure F = RiscVFrame
  structure M = Temp
  structure R = Tree

  (* fix these ... they are clearly insufficient *)

  datatype level = TopLevel
                 | InnerLevel of { parent : level
                            , frame : Frame.frame
                            , unique : unit ref}

  type access = level * Frame.access

  val outermost = TopLevel
  fun newLevel { parent, name, formals } = let 
                                               val frame = Frame.newFrame{name=name, formals=true::formals}
                                           in
                                               InnerLevel { parent = parent
                                                          , frame = frame
                                                          , unique = ref() }
                                           end

  fun formals level = (case level of TopLevel => []
                                   | InnerLevel {parent, frame, unique} => let 
                                                                              val accList = List.tl (Frame.formals(frame))
                                                                           in
                                                                              map(fn accesses => (level,accesses)) accList
                                                                           end)
  fun allocLocal level escape = (case level 
                                   of TopLevel => E.impossible "this is outermost!"
                                    | InnerLevel {parent, frame, unique} => (level, Frame.allocLocal frame escape))

  fun newBreakPos() = M.newlabel()

    datatype exp = Ex of R.exp
                 | Nx of R.stm
                 | Cx of M.label * M.label -> R.stm


    datatype frag = PROC of { body : R.stm
                            , frame : F.frame }
                  | STRING of Temp.label * string
    val fragments = ref([]:frag list)
    fun initResult _ = (ref([]:frag list) := [])
    fun getResult _ = !fragments


    val error =
    Ex(R.CONST(0))
(* [] represent nothing -> CONST 0 *)
   fun seq expList =
         case expList of
           []  => R.EXP(R.CONST 0)
          |head :: [] => head
          |head :: tail => R.SEQ(head,seq(tail))

  fun unEx (Ex e) = e
    | unEx (Cx genstm) =
      let 
          val r = M.newtemp()
          val t = M.newlabel()
          val f = M.newlabel()
          fun seq expList =
            case expList of
              []  => R.EXP(R.CONST 0)
              |head :: [] => head
              |head :: tail => R.SEQ(head,seq(tail))
      in R.ESEQ(seq[R.MOVE(R.TEMP r, R.CONST 1),
                    genstm(t,f),
                    R.LABEL f,
                    R.MOVE(R.TEMP r, R.CONST 0),
                    R.LABEL t],
                  R.TEMP r)
      end
    | unEx (Nx s) = R.ESEQ(s, R.CONST 0)

  fun unCx (Cx c) = c
    (* CONST 0 AND CONST 1 *)
    | unCx (Ex (R.CONST 0)) = (fn (t,f) => R.JUMP(R.NAME f,[f]))
    | unCx (Ex (R.CONST 1)) = (fn (t,f) => R.JUMP(R.NAME t,[t]))
    | unCx (Ex e) = (fn (t,f) => R.CJUMP(R.EQ, e, R.CONST 1, t, f))
    (* shoud not happen *)
    | unCx (Nx _) = raise Fail("Should not happen!")

  fun unNx (Nx n) = n
    | unNx (Ex e) = R.EXP e
    | unNx (Cx c) = 
      let
        val t = M.newlabel()
        val f = M.newlabel()
      in
        seq[c(t,f), R.LABEL t, R.LABEL f]
      end


  fun opers (left, A.PlusOp, right)  =
        Ex(R.BINOP(R.PLUS, unEx(left), unEx(right)))
      |opers (left,A.MinusOp,  right) =
        Ex(R.BINOP(R.MINUS, unEx(left), unEx(right)))
      |opers (left, A.TimesOp, right) =
        Ex(R.BINOP(R.MUL, unEx(left), unEx(right)))
      |opers (left, A.DivideOp, right) =
        Ex(R.BINOP(R.DIV, unEx(left), unEx(right)))
      |opers (left, A.EqOp, right)  =
        Cx(fn(t,f)=>R.CJUMP(R.EQ, unEx(left), unEx(right), t, f))
      |opers (left, A.NeqOp, right) =
        Cx(fn(t,f)=>R.CJUMP(R.NE, unEx(left), unEx(right), t, f))
      |opers (left, A.LtOp, right) =
        Cx(fn(t,f)=>R.CJUMP(R.LT, unEx(left), unEx(right), t, f))
      |opers (left, A.LeOp, right) =
        Cx(fn(t,f)=>R.CJUMP(R.LE, unEx(left), unEx(right), t, f))
      |opers (left, A.GtOp, right)  =
        Cx(fn(t,f)=>R.CJUMP(R.GT, unEx(left), unEx(right), t, f))
      |opers (left, A.GeOp, right) =
        Cx(fn(t,f)=>R.CJUMP(R.GE, unEx(left), unEx(right), t, f))
(*
– string operations
– string memory allocation
Frame.externalCall: string * Tree.exp -> Tree.exp
Frame.externalCall("stringEqual", [s1, s2])
– Implementation takes into account calling conventions of external functions. – Easiest implementation:
fun externalCall(s, args) = T.CALL(T.NAME(Temp.namedlabel(s)), args)
*)
  fun stringOpers (left, A.EqOp, right)  =
          Ex(Frame.externalCall("stringEqual",[unEx(left), unEx(right)]))
      |stringOpers (left, A.NeqOp, right) =
          Ex(Frame.externalCall("stringNotEqual",[unEx(left), unEx(right)]))
      |stringOpers (left, A.LtOp, right) =
          Ex(Frame.externalCall("stringLessThan",[unEx(left), unEx(right)]))
      |stringOpers (left, A.LeOp, right) =
          Ex(Frame.externalCall("stringLessOrEqual",[unEx(left), unEx(right)]))
      |stringOpers (left, A.GtOp, right)  =
          Ex(Frame.externalCall("stringGreterThan",[unEx(left), unEx(right)]))
      |stringOpers (left, A.GeOp, right) =
          Ex(Frame.externalCall("stringGreterOrEqual",[unEx(left), unEx(right)]))

  fun assign (var,exp) =
        Nx(R.MOVE(unEx(var), unEx(exp)))


  fun intExp(i) =
        Ex(R.CONST(i))

  fun boolExp(b) =
        case b of 
          true => Ex(R.CONST(1))
          |false => Ex(R.CONST(0))


  (* type rectype = {f1:int, f2:int, f3:int} 
                        |       |       | 
                offset: 0       1      2
      var a:rectype := rectype{f1=4, f2=5, f3=6}
      Let e be IR tree for a: 
      a.f3:
      MEM(BINOP(PLUS, e, BINOP(MUL, CONST(3), CONST(w)))) 
      need to pass list parameter in semant.sml*)
  fun fieldVar(var,symbol,list1) =
    let
        fun findOffset (symbol,num,tlist) =
          let 
              fun head(tlist) = 
                case tlist of
                  [] => raise Fail("Empty list")
                 |(head::[]) => head
                 |(head::tail) => head
              fun tail(tlist) =
                case tlist of 
                  [] => raise Fail("Empty list")
                 |(head::[]) => raise Fail("No this field")
                 |(head::tail) => tail
          in
            if symbol = head(tlist) then num
            else findOffset(symbol,num+1,tail(tlist))
          end   
    in
      Ex(R.MEM(
          R.BINOP(
            R.PLUS,unEx(var),R.BINOP(
              R.MUL,R.CONST(findOffset(symbol,1,list1)),R.CONST(Frame.wordSize))))
        )
    end
  (* MEM(BINOP(PLUS, e, BINOP(MUL, i, CONST(w)))) *)
  fun subscriptVar(arr, i) =
      Ex(R.MEM(
        R.BINOP(
          R.PLUS, unEx(arr),R.BINOP(
            R.MUL,unEx(i),R.CONST(Frame.wordSize)))))

(* seq decs in let, then body*)
  fun letExp (decsList, body) = 
    Ex(R.ESEQ(seq(map unNx decsList), unEx body))

  val nilExp  = 
    Ex(R.CONST(0))

  (* seq seqexp *)
  fun seqExp (expList) = 
    Nx(seq(map unNx expList))

 fun break breakLabel = 
    Nx(R.JUMP(R.NAME breakLabel, [breakLabel]))

(* type intarray = array of int
var a:intarray := intarray[10] of 7
Call run-time system function initArray to malloc and initialize array.*)
  fun arrayExp (size, init) =
    Ex(R.CALL(R.NAME(M.namedlabel("initArray")),[unEx(size),unEx(init)]))

(* if e1 then e2 else e3
• Treat e1 as Cx expression ⇒ apply unCx.
• Treat e2, e3 as Ex expressions ⇒ apply unEx.
Ex(ESEQ(SEQ(unCx(e1)(t, f), SEQ(LABEL(t),SEQ(MOVE(TEMP(r), unEx(e2)), SEQ(JUMP(NAME(join)),
SEQ(LABEL(f), SEQ(MOVE(TEMP(r), unEx(e3)), LABEL(join)))))))
TEMP(r))) *)
  fun ifExp(test,then1) =
        let
          val t = M.newlabel()
          val f = M.newlabel()
          val r = M.newtemp()
        in
          case then1 of
            Nx(then') =>
                Nx(seq[(unCx(test))(t,f),
                R.LABEL(t),
                unNx(then1),
                R.LABEL(f)])
            |Ex(then') =>
              let
                val join = M.newlabel()
              in
                Ex(R.ESEQ(seq[(unCx(test))(t,f),
                R.LABEL(t),
                R.MOVE(R.TEMP r, unEx(then1)),
                R.LABEL(f)],
                R.TEMP r))
              end
            |Cx(then') => 
                Cx(fn (t1,f1) =>
                seq[(unCx(test))(t,f),
                R.LABEL(t),
                (unCx(then1))(t1,f1),
                R.LABEL(f)])
        end
    fun ifElse(test,then1,else1)=
        let
          val t = M.newlabel()
          val f = M.newlabel()
          val r = M.newtemp()
        in
        case then1 of
          Nx(then') =>
              let 
                val join = M.newlabel()
              in
              Nx(seq[(unCx(test))(t,f),
              R.LABEL(t),
              unNx(then1),
              R.JUMP(R.NAME(join), [join]),
              R.LABEL(f),
              unNx(else1),
              R.LABEL(join)])
              end
          |Ex(then') =>
              let 
                val join = M.newlabel()
              in
              Ex(R.ESEQ(seq[(unCx(test))(t,f),
              R.LABEL(t),
              R.MOVE(R.TEMP r, unEx(then1)),
              R.JUMP(R.NAME(join), [join]),
              R.LABEL(f),
              R.MOVE(R.TEMP r, unEx(else1)),
              R.LABEL(join)],
              R.TEMP r))
              end
          |Cx(then') => 
              Cx(fn (t1,f1) =>
                seq[(unCx(test))(t,f),
                R.LABEL(t),
                (unCx(then1))(t1,f1),
                R.LABEL(f),
                (unCx(else1)(t1,f1))
                ])
        end
(*test:
    if not(CONDITION) goto done
    BODY
    goto test
done: *)
  fun whileExp(test,body,done)=
    let
      val testLable = M.newlabel()
      val bodyLable = M.newlabel()
    in
      Nx(seq[R.LABEL testLable,
            (unCx(test))(bodyLable,done),
              R.LABEL bodyLable,
              unNx(body),
              R.JUMP(R.NAME testLable, [testLable]),
              R.LABEL done])
    end

(* 
var i := lo
for:
    if i<hi do
body:
    body
 i := i + 1)
escape
*)
  fun for(var,escape,lo,hi,body) =
    let
      val bodyLable = M.newlabel()
      val forLable = M.newlabel()
    in
      Nx(seq[R.MOVE(unEx(var),unEx(lo)),
        R.LABEL forLable,
        R.CJUMP(R.LE,unEx(var),unEx(hi),bodyLable,escape),
        R.LABEL bodyLable,
        unNx(body),
        R.MOVE(unEx(var),R.BINOP(R.PLUS,unEx(var),R.CONST(1))),
        R.JUMP(R.NAME forLable, [forLable]),
        R.LABEL escape])
    end


(* 
Call an external memory allcation function retuns a
 pointer to an n word area in to a new temporary r. 
 Then a seroes pf ,PVE trees can initialize offsets 
 from r with the translations of expressions e1, finally 
 the result of the whole expression is TEMP(r)
type rectype = { f1:int, f2:int, f3:int }
var a:rectype := rectype{f1 = 4, f2 = 5, f3 = 6}
ESEQ(SEQ( MOVE(TEMP(result),
            Frame.externalCall("allocRecord",
[CONST(12)])),
SEQ( MOVE(BINOP(PLUS, TEMP(result), CONST(0*w)),
CONST(4)),
SEQ( MOVE(BINOP(PLUS, TEMP(result), CONST(1*w)),
CONST(5)),
SEQ( MOVE(BINOP(PLUS, TEMP(result), CONST(2*w)),
          CONST(6)))))),
TEMP(result)) *)
  fun record(fields) =
    let
      val r = M.newtemp()
      fun addRecords(fields,num)=
        case fields of
          []=>nil
          |head::[] => 
            R.MOVE(
              R.BINOP(
                  R.PLUS, 
                  R.TEMP r, 
                  R.CONST(num*Frame.wordSize)),
              unEx(head)) :: addRecords([],0)
          |head::tail =>
            R.MOVE(
              R.BINOP(
                  R.PLUS, 
                  R.TEMP r, 
                  R.CONST(num*Frame.wordSize)),
              unEx(head)) :: addRecords(tail,num+1)
    in
      Ex(R.ESEQ(seq(
        R.MOVE(
          R.TEMP r, 
          Frame.externalCall("allocRecord",[R.CONST(Frame.wordSize * length(fields))])
        )::
          addRecords(fields,1)
        ),
      R.TEMP r))
    end

  fun checkLevel(level1, level2) = case level1 
                                     of TopLevel => (case level2 
                                                       of TopLevel => true
                                                        | _ => false )
                                      | InnerLevel{parent,frame,unique} => (case level2
                                                                              of InnerLevel{parent,frame,unique} => true
                                                                              | _ => false)

  fun computeSL(calleeLevel,callerLevel,fp) = case callerLevel
    of InnerLevel{parent,frame,unique} =>
      if checkLevel(calleeLevel,callerLevel) then fp
      else
          let 
             val staticLink = List.hd (Frame.formals(frame))
          in 
             computeSL(calleeLevel,parent,Frame.exp(staticLink) fp) 
          end
    | TopLevel =>
      (ErrorMsg.impossible "No SL!")

(*  
f(a1, a2, ..., an) =>
CALL(NAME(l_f), sl::[e1, e2, ..., en])
sl static link of f (computable at compile-time) To compute static link, need:
– l f : level of f
– l g : level of g, the calling function
Computation similar to simple variable access.

*)
  fun callExp(funcLabel, args, expLevel, funcLevel) = case funcLevel of TopLevel => ErrorMsg.impossible "Wrong function level!"
                                                                   | InnerLevel{parent=TopLevel, frame, unique} =>
                                                                               Ex(Frame.externalCall(Symbol.name(funcLabel), (map unEx args)))
                                                                   | _ => Ex(R.CALL(R.NAME(funcLabel), computeSL(funcLevel,expLevel,R.TEMP(Frame.FP))::(map unEx args)))


(* 
  All string operations performed by run-time system functions.
  In Tiger, C, string literal is constant address of memory segment initialized to char-
– Label definition includes directives that reserve and initialize memory.
‘‘foo’’:
1. Translate module creates new label   .
2. Tree.NAME() returned: used to refer to string.
3. String fragment “foo” created with label   . Fragment is handed to code emitter, 
  which emits directives to initialize memory with the characters of “foo” at address  *)
fun stringExp(s) =
  Ex(R.CONST(0))


(* access,level -> exp .access:infram(offset) inreg(regName)*) 
  fun simpleVar(access, level) =
      let 
        val (level1, accessExp) = access
      in
        Ex(Frame.exp(accessExp)(computeSL(level,level1,R.TEMP(Frame.FP))))

      end





end
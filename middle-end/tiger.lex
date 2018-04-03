(* tiger scanner *)

type pos = int;             					(* %pos int given in tiger.grm *)
type svalue = Tokens.svalue
type ('a, 'b) token = ('a, 'b) Tokens.token
type lexresult = (svalue, pos) token;

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos

fun nextLn pos = ( lineNum := !lineNum+1
		           ; linePos := pos :: !linePos
                 )

fun ctrlAlphaU c = chr ((ord c) - (ord #"A") + 1)

fun ctrlAlphaL c = chr ((ord c) - (ord #"a") + 1)

fun ctrlChar c = let fun tr #"[" = 27
		                 | tr #"\\" = 28
		                 | tr #"]" = 29
		                 | tr #"^" = 30
		                 | tr #"_" = 31
		                 | tr #"@" = 0
		                 | tr _ = ( ErrorMsg.impossible "CTRL CHAR CONSISTENCY"
				                    ; 0)
		           in chr (tr c)
		           end

val strBuf : (pos * string option) ref = ref (0, NONE)

fun strApp s = case !strBuf
                of (_, NONE) => ErrorMsg.impossible "STRING CONSISTENCY"
		           | (l, SOME s') => strBuf := (l, SOME (s' ^ s))

fun err (p1,p2) = ErrorMsg.error p1

val comments : pos list ref = ref []
			      
fun eof () = let val pos = hd(!linePos)
	          in ( case !comments
                   of [] => ()
		              | [c] => ErrorMsg.error pos ("Unfinished comment starting at line " ^ (Int.toString c))
		              | (c::cs) => let fun loop []      = ( ErrorMsg.impossible "COMMENT CONSISTENCY"
						                                      ; "bogus"
                                                        )
				                           | loop [c]     = " and " ^ Int.toString c
				                           | loop (c::cs) = ", " ^ Int.toString c ^ loop cs
				                     in ErrorMsg.error pos ("Unfinished comments from lines " ^ Int.toString c ^ loop cs)
				                     end
		          ; comments := []
		          ; case !strBuf of
		                (_, NONE) => ()
		              | (l, SOME _) => ErrorMsg.error pos ("Unfinished string started at line " ^ (Int.toString l))
		          ; strBuf := (0, NONE)
		          ; Tokens.EOF(pos,pos) )
	          end

%% 
nonzero=[1-9];
digit=[0-9];
alpha=[a-zA-Z];
alphanum=[a-zA-Z0-9_];
whitespace=[ \009\011\012\013];
ctrl="\\^";
ctrlch={ctrl}[\\@^_[\]];

%s INITIAL COMMENT STRING FMTSEQ;

%header (functor TigerLexFun(structure Tokens : Tiger_TOKENS)  (* %name Tiger given in tiger.grm *));

%%
<INITIAL>","	=> (Tokens.COMMA(yypos,yypos + size yytext));
<INITIAL>":"	=> (Tokens.COLON(yypos,yypos + size yytext));
<INITIAL>";"	=> (Tokens.SEMICOLON(yypos,yypos + size yytext));
<INITIAL>"("	=> (Tokens.LPAREN(yypos,yypos + size yytext));
<INITIAL>")"	=> (Tokens.RPAREN(yypos,yypos + size yytext));
<INITIAL>"["	=> (Tokens.LBRACK(yypos,yypos + size yytext));
<INITIAL>"]"	=> (Tokens.RBRACK(yypos,yypos + size yytext));
<INITIAL>"{"	=> (Tokens.LBRACE(yypos,yypos + size yytext));
<INITIAL>"}"	=> (Tokens.RBRACE(yypos,yypos + size yytext));
<INITIAL>"."	=> (Tokens.DOT(yypos,yypos + size yytext));
<INITIAL>"+"	=> (Tokens.PLUS(yypos,yypos + size yytext));
<INITIAL>"-"	=> (Tokens.MINUS(yypos,yypos + size yytext));
<INITIAL>"*"	=> (Tokens.TIMES(yypos,yypos + size yytext));
<INITIAL>"/"	=> (Tokens.DIVIDE(yypos,yypos + size yytext));
<INITIAL>"="	=> (Tokens.EQ(yypos,yypos + size yytext));
<INITIAL>"<>"	=> (Tokens.NEQ(yypos,yypos + size yytext));
<INITIAL>"<"	=> (Tokens.LT(yypos,yypos + size yytext));
<INITIAL>"<="	=> (Tokens.LE(yypos,yypos + size yytext));
<INITIAL>">"	=> (Tokens.GT(yypos,yypos + size yytext));
<INITIAL>">="	=> (Tokens.GE(yypos,yypos + size yytext));
<INITIAL>"&"	=> (Tokens.AND(yypos,yypos + size yytext));
<INITIAL>"|"	=> (Tokens.OR(yypos,yypos + size yytext));
<INITIAL>":="	=> (Tokens.ASSIGN(yypos,yypos + size yytext));

<INITIAL>true	=> (Tokens.TRUE(yypos,yypos + size yytext));
<INITIAL>false	=> (Tokens.FALSE(yypos,yypos + size yytext));

<INITIAL>var  	=> (Tokens.VAR(yypos,yypos + size yytext));
<INITIAL>while	=> (Tokens.WHILE(yypos,yypos + size yytext));
<INITIAL>for	=> (Tokens.FOR(yypos,yypos + size yytext));
<INITIAL>to   	=> (Tokens.TO(yypos,yypos + size yytext));
<INITIAL>break	=> (Tokens.BREAK(yypos,yypos + size yytext));
<INITIAL>let	=> (Tokens.LET(yypos,yypos + size yytext));
<INITIAL>in 	=> (Tokens.IN(yypos,yypos + size yytext));
<INITIAL>end	=> (Tokens.END(yypos,yypos + size yytext));
<INITIAL>type	=> (Tokens.TYPE(yypos,yypos + size yytext));
<INITIAL>array	=> (Tokens.ARRAY(yypos,yypos + size yytext));
<INITIAL>if 	=> (Tokens.IF(yypos,yypos + size yytext));
<INITIAL>then	=> (Tokens.THEN(yypos,yypos + size yytext));
<INITIAL>else	=> (Tokens.ELSE(yypos,yypos + size yytext));
<INITIAL>do 	=> (Tokens.DO(yypos,yypos + size yytext));
<INITIAL>of 	=> (Tokens.OF(yypos,yypos + size yytext));
<INITIAL>nil	=> (Tokens.NIL(yypos,yypos + size yytext));
<INITIAL>function	=> (Tokens.FUNCTION(yypos,yypos + size yytext));

<INITIAL>({alpha}{alphanum}*)	=> (Tokens.ID(yytext, yypos, yypos + size yytext));
<INITIAL>({digit}{digit}*)	=> (case Int.fromString yytext
                               of NONE => ( ErrorMsg.error yypos ("number not integer " ^ yytext)
                                          ; continue()
                                          )
                               | SOME z => Tokens.INT(z,yypos,yypos +size yytext)
                              );

<INITIAL>"/*"	=> ( YYBEGIN COMMENT
		            ; comments := yypos :: !comments
		            ; continue());

<COMMENT>"/*"	=> ( comments := yypos :: !comments
                  ; continue()
                  );

<COMMENT>\n	=> ( nextLn yypos
               ; continue()
               );

<COMMENT>"*/"	=> ( case !comments
                    of [] => ( YYBEGIN INITIAL
                             ; ErrorMsg.impossible "COMMENTS STATE CONSISTENCY"
                             )
                    | [c] => ( YYBEGIN INITIAL
                             ; comments := []
                             )
                    | (c::cs) => comments := cs
                  ; continue()
                  );

<COMMENT>.	=> (continue());

<INITIAL>\"	=> ( YYBEGIN STRING
               ; strBuf := (!lineNum, SOME "")
               ; continue()
               );

<STRING>\"	=> ( YYBEGIN INITIAL
               ; case !strBuf
                  of (_, NONE) => ( ErrorMsg.impossible "STRING STATE CONSISTENCY"
                                  ; continue()
                                  )
                   | (l, SOME s) => ( strBuf := (0, NONE)
                                    ; Tokens.STRING(s, l, size s)
                                    )
               );

<STRING>\\n	=> ( strApp "\n"
               ; nextLn yypos
               ; continue()
               );

<STRING>\\t	=> ( strApp "\t"
               ; continue()
               );

<STRING>\\\"	=> ( strApp "\\"
                  ; continue()
                  );

<STRING>({ctrl}[a-z])	=> ( strApp (str (ctrlAlphaL (String.sub (yytext, 2))))
                           ; continue()
                           );

<STRING>({ctrl}[A-Z])	=> ( strApp (str (ctrlAlphaU (String.sub (yytext, 2))))
                           ; continue()
                           );

<STRING>({ctrlch})	=> ( strApp (str (ctrlChar (String.sub (yytext, 2))))
                        ; continue()
                        );

<STRING>(\\{digit}{digit}{digit})	=> ( case Int.fromString (substring (yytext, 1, 3))
                                         of NONE   => ErrorMsg.impossible "DECIMAL CONTROL STRING CONSISTENCY"
                                          | SOME n => if n>=0 andalso n<128
                                                      then strApp (str (chr n))
                                                      else ErrorMsg.error yypos "invalid decimal control character"
                                       ; continue()
                                       );

<STRING>(\\{whitespace})	=> ( YYBEGIN FMTSEQ
				                  ; continue()
                              );

<STRING>\\\n		=> ( YYBEGIN FMTSEQ
			            ; continue()
                     );

<STRING>\\\\	=> ( strApp "\\"
		            ; continue()
                  );

<STRING>\\	=> ( ErrorMsg.error yypos "invalid escape sequence"
		         ; continue()
               );

<STRING>\n	=> ( ErrorMsg.error yypos "newline in string constant"
		         ; continue()
               );

<STRING>.	=> ( strApp yytext
		         ; continue()
               );


<FMTSEQ>({whitespace})	=> (continue());

<FMTSEQ>\n	=> (continue());

<FMTSEQ>\\	=> ( YYBEGIN STRING
		         ; continue()
               );

<FMTSEQ>.	=> ( YYBEGIN STRING
		         ; ErrorMsg.error yypos ("illegal format character " ^ yytext)
		         ; strApp yytext
		         ; continue()
               );

<INITIAL>\n	=> ( nextLn yypos
		         ; continue()
               );
			
<INITIAL>({whitespace}) => (continue());

<INITIAL>.     	=> ( ErrorMsg.error yypos ("illegal character " ^ yytext)
		               ; continue()
                     );
		


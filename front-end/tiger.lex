(* tiger scanner *)

type pos = int;             					(* %pos int given in tiger.grm *)
type svalue = Tokens.svalue
type ('a, 'b) token = ('a, 'b) Tokens.token
type lexresult = (svalue, pos) token;

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos

fun err (p1,p2) = ErrorMsg.error p1

fun eof () = let val pos = hd(!linePos)
	          in Tokens.EOF(pos,pos)
	          end

fun nextLn pos = ( lineNum := !lineNum+1
		           ; linePos := pos :: !linePos
                 )

%% 
%header (functor TigerLexFun(structure Tokens : Tiger_TOKENS)  (* %name Tiger given in tiger.grm *));

%%
\n	=> ( nextLn yypos
		; continue()
      );
			
","	=> (Tokens.COMMA(    yypos,yypos + (size yytext)));
var  	=> (Tokens.VAR(      yypos,yypos + (size yytext)));
"123"	=> (Tokens.INT(123,  yypos,yypos + (size yytext)));
.     => ( ErrorMsg.error yypos ("illegal character " ^ yytext)
         ; continue()
         );

		


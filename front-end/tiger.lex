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
digits = [1-9]*[0-9]+;
comments =\/\*.*\*\/;
quote = [\"];
nonQuote=[^\"];
whitespace = [\ \t]+;
%header (functor TigerLexFun(structure Tokens : Tiger_TOKENS)  (* %name Tiger given in tiger.grm *));

%%


{digits} => (Tokens.INT(valOf (Int.fromString yytext),yypos,yypos+size yytext));
{quote}{nonQuote}*{quote} => (Tokens.STRING(      yytext, yypos, yypos + (size yytext)));
"," =>  (Tokens.COMMA(       yypos,yypos + (size yytext)));
":" =>  (Tokens.COLON(       yypos,yypos + (size yytext)));
";" =>  (Tokens.SEMICOLON(   yypos,yypos + (size yytext)));
"(" =>  (Tokens.LPAREN(      yypos,yypos + (size yytext)));
")" =>  (Tokens.RPAREN(      yypos,yypos + (size yytext)));
"[" =>  (Tokens.LBRACK(      yypos,yypos + (size yytext)));
"]" =>  (Tokens.RBRACK(      yypos,yypos + (size yytext)));
"{" =>  (Tokens.LBRACE(      yypos,yypos + (size yytext)));
"}" =>  (Tokens.RBRACE(      yypos,yypos + (size yytext)));
"." =>  (Tokens.DOT(         yypos,yypos + (size yytext)));
"+" =>  (Tokens.PLUS(        yypos,yypos + (size yytext)));
"-" =>  (Tokens.MINUS(       yypos,yypos + (size yytext)));
"*" =>  (Tokens.TIMES(       yypos,yypos + (size yytext)));
"/" =>  (Tokens.DIVIDE(      yypos,yypos + (size yytext)));
"=" =>  (Tokens.EQ(          yypos,yypos + (size yytext)));
"<>" => (Tokens.NEQ(         yypos,yypos + (size yytext)));
"<" =>  (Tokens.LT(          yypos,yypos + (size yytext)));
"<=" => (Tokens.LE(          yypos,yypos + (size yytext)));
">" =>  (Tokens.GT(          yypos,yypos + (size yytext)));
">=" => (Tokens.GE(          yypos,yypos + (size yytext)));
"&" =>  (Tokens.AND(         yypos,yypos + (size yytext)));
"|" =>  (Tokens.OR(          yypos,yypos + (size yytext)));
":="=>  (Tokens.ASSIGN(      yypos,yypos + (size yytext)));
if =>   (Tokens.IF(          yypos,yypos + (size yytext)));
then => (Tokens.THEN(        yypos,yypos + (size yytext)));
else => (Tokens.ELSE(        yypos,yypos + (size yytext)));
while =>(Tokens.WHILE(       yypos,yypos + (size yytext)));
for =>  (Tokens.FOR(         yypos,yypos + (size yytext)));
to =>   (Tokens.TO(          yypos,yypos + (size yytext)));
do =>   (Tokens.DO(          yypos,yypos + (size yytext)));
let =>  (Tokens.LET(         yypos,yypos + (size yytext)));
in =>   (Tokens.IN(          yypos,yypos + (size yytext)));
end =>  (Tokens.END(         yypos,yypos + (size yytext)));
of =>   (Tokens.OF(          yypos,yypos + (size yytext)));
break =>(Tokens.BREAK(       yypos,yypos + (size yytext)));
nil =>  (Tokens.NIL(         yypos,yypos + (size yytext)));
function =>(Tokens.FUNCTION( yypos,yypos + (size yytext)));
var =>  (Tokens.VAR(         yypos,yypos + (size yytext)));
type => (Tokens.TYPE(        yypos,yypos + (size yytext)));
array =>(Tokens.ARRAY(       yypos,yypos + (size yytext)));
[a-zA-Z][_a-zA-Z0-9]* => (Tokens.ID(yytext, yypos, yypos+(size yytext)));
{comments} => (              continue());
{whitespace} => (            continue());
\n  => ( nextLn yypos
    ; continue()
      );
.     => ( ErrorMsg.error yypos ("illegal character " ^ yytext)
; continue()
);
		


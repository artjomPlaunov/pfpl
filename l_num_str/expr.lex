structure Tokens = Tokens

type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult= (svalue,pos) token
fun getstring s =
   let
    val n = size s
   in
     substring (s, 1, n-2)
   end

val pos = ref 0
val eof = fn () => Tokens.EOF(!pos,!pos)

exception Illegal_character of pos

(*
structure SymTab = SplayMapFn(struct type ord_key = string
                                     val compare = String.compare
                              end)

val env : Var.t SymTab.map ref = ref SymTab.empty

fun getvar string =
  case SymTab.find(!env, string) of
    SOME v => v
  | NONE => let val v = Var.newvar string
            in
              (env := SymTab.insert(!env, string, v);
               v)
            end

fun setEnv e = env := e

(* {alpha}{any}* => (Tokens.IDENT((getvar yytext),!pos,!pos)); *)

*)

%%
%header (functor Exp_LexFun(structure Tokens: Exp_TOKENS));

alpha=[A-Za-z];
digit=[0-9];
any = [@a-zA-Z0-9_];

ws = [\ \t];

%%
\n       => (pos := (!pos) + 1; lex());
{ws}+    => (lex());
{digit}+ => (Tokens.NUM( let val SOME(n) = (Int.fromString yytext) in n end,!pos,!pos));
"\""{any|" "}*"\"" => (Tokens.STR(getstring(yytext),!pos,!pos));
"+"      => (Tokens.PLUS(!pos,!pos));
"*"      => (Tokens.TIMES(!pos,!pos));
"^"      => (Tokens.CARET(!pos,!pos));
":"      => (Tokens.COLON(!pos,!pos));
";"      => (Tokens.SEMI(!pos,!pos));
"="      => (Tokens.EQUALS(!pos,!pos));
"step"   => (Tokens.STEP(!pos,!pos));
"eval"   => (Tokens.EVAL(!pos,!pos));
"checked"=> (Tokens.CHECKED(!pos,!pos));
"unchecked"=> (Tokens.UNCHECKED(!pos,!pos));
"num"    => (Tokens.NUMTYPE(!pos,!pos));
"str"    => (Tokens.STRTYPE(!pos,!pos));
"let"    => (Tokens.LET(!pos,!pos));
"len"    => (Tokens.LEN(!pos,!pos));
"val"    => (Tokens.VAL(!pos,!pos));
"in"     => (Tokens.IN(!pos,!pos));
"end"    => (Tokens.END(!pos,!pos));
"("      => (Tokens.LPAREN(!pos,!pos));
")"      => (Tokens.RPAREN(!pos,!pos));
{alpha}{any}* => (Tokens.IDENT(yytext,!pos,!pos));

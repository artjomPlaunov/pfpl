functor ExpLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Exp_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
structure T = Term
structure TY = Type
structure TLC = TopLevelCommands
exception Parse of string

fun createlet (e1,var) e2 =  T.$$(TermOps.Let, [e1, T.\\(var, e2)])


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\000\000\012\000\000\000\000\000\
\\001\000\002\000\020\000\003\000\019\000\005\000\018\000\006\000\017\000\
\\010\000\016\000\016\000\015\000\000\000\
\\001\000\002\000\034\000\000\000\
\\001\000\004\000\038\000\007\000\027\000\008\000\026\000\009\000\025\000\000\000\
\\001\000\013\000\040\000\000\000\
\\001\000\017\000\024\000\000\000\
\\001\000\017\000\024\000\018\000\031\000\000\000\
\\001\000\019\000\033\000\000\000\
\\001\000\020\000\006\000\021\000\005\000\000\000\
\\043\000\000\000\
\\044\000\000\000\
\\045\000\007\000\027\000\008\000\026\000\009\000\025\000\000\000\
\\046\000\022\000\009\000\023\000\008\000\000\000\
\\047\000\007\000\027\000\008\000\026\000\009\000\025\000\000\000\
\\048\000\022\000\009\000\023\000\008\000\000\000\
\\049\000\000\000\
\\050\000\000\000\
\\051\000\008\000\026\000\000\000\
\\052\000\000\000\
\\053\000\007\000\027\000\008\000\026\000\000\000\
\\054\000\000\000\
\\055\000\000\000\
\\056\000\000\000\
\\057\000\000\000\
\\058\000\000\000\
\\059\000\000\000\
\\060\000\000\000\
\\061\000\000\000\
\\062\000\000\000\
\\063\000\007\000\027\000\008\000\026\000\009\000\025\000\000\000\
\\064\000\007\000\027\000\008\000\026\000\009\000\025\000\000\000\
\\065\000\000\000\
\\066\000\000\000\
\"
val actionRowNumbers =
"\008\000\009\000\010\000\014\000\
\\012\000\001\000\016\000\015\000\
\\001\000\005\000\024\000\023\000\
\\013\000\027\000\001\000\022\000\
\\021\000\001\000\032\000\011\000\
\\006\000\007\000\002\000\001\000\
\\001\000\001\000\020\000\003\000\
\\028\000\001\000\026\000\031\000\
\\004\000\019\000\018\000\017\000\
\\025\000\029\000\001\000\030\000\
\\000\000"
val gotoT =
"\
\\001\000\040\000\002\000\002\000\003\000\001\000\000\000\
\\000\000\
\\000\000\
\\004\000\005\000\000\000\
\\004\000\008\000\000\000\
\\005\000\012\000\006\000\011\000\007\000\010\000\008\000\009\000\000\000\
\\000\000\
\\000\000\
\\005\000\019\000\006\000\011\000\007\000\010\000\008\000\009\000\000\000\
\\009\000\021\000\010\000\020\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\026\000\006\000\011\000\007\000\010\000\008\000\009\000\000\000\
\\000\000\
\\000\000\
\\005\000\027\000\006\000\011\000\007\000\010\000\008\000\009\000\000\000\
\\000\000\
\\000\000\
\\009\000\028\000\010\000\020\000\000\000\
\\011\000\030\000\000\000\
\\000\000\
\\005\000\033\000\006\000\011\000\007\000\010\000\008\000\009\000\000\000\
\\005\000\034\000\006\000\011\000\007\000\010\000\008\000\009\000\000\000\
\\005\000\035\000\006\000\011\000\007\000\010\000\008\000\009\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\037\000\006\000\011\000\007\000\010\000\008\000\009\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\039\000\006\000\011\000\007\000\010\000\008\000\009\000\000\000\
\\000\000\
\\000\000\
\"
val numstates = 41
val numrules = 24
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | STR of unit ->  (string) | NUM of unit ->  (int)
 | IDENT of unit ->  (string) | letend of unit ->  (unit)
 | let2 of unit ->  (T.t*Var.t) | let1 of unit ->  (T.t)
 | letstart of unit ->  (unit) | letexp of unit ->  (T.t)
 | var of unit ->  (T.t) | exp of unit ->  (T.t)
 | mode of unit ->  (TLC.dynMode) | step of unit ->  (TLC.cmd)
 | eval of unit ->  (TLC.cmd) | start of unit ->  (TLC.cmd)
end
type svalue = MlyValue.svalue
type result = TLC.cmd
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 0) => true | (T 11) => true | _ => false
val showTerminal =
fn (T 0) => "EOF"
  | (T 1) => "IDENT"
  | (T 2) => "LPAREN"
  | (T 3) => "RPAREN"
  | (T 4) => "NUM"
  | (T 5) => "STR"
  | (T 6) => "PLUS"
  | (T 7) => "TIMES"
  | (T 8) => "CARET"
  | (T 9) => "LEN"
  | (T 10) => "COLON"
  | (T 11) => "SEMI"
  | (T 12) => "EQUALS"
  | (T 13) => "NUMTYPE"
  | (T 14) => "STRTYPE"
  | (T 15) => "LET"
  | (T 16) => "VAL"
  | (T 17) => "IN"
  | (T 18) => "END"
  | (T 19) => "STEP"
  | (T 20) => "EVAL"
  | (T 21) => "CHECKED"
  | (T 22) => "UNCHECKED"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16)
 $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9)
 $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 3) $$ (T 2) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.step step1, step1left, step1right)) :: 
rest671)) => let val  result = MlyValue.start (fn _ => let val  (step
 as step1) = step1 ()
 in (step)
end)
 in ( LrTable.NT 0, ( result, step1left, step1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.eval eval1, eval1left, eval1right)) :: 
rest671)) => let val  result = MlyValue.start (fn _ => let val  (eval
 as eval1) = eval1 ()
 in (eval)
end)
 in ( LrTable.NT 0, ( result, eval1left, eval1right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: ( _, ( 
MlyValue.mode mode1, _, _)) :: ( _, ( _, STEP1left, _)) :: rest671))
 => let val  result = MlyValue.step (fn _ => let val  (mode as mode1)
 = mode1 ()
 val  (exp as exp1) = exp1 ()
 in (TLC.Step(SOME(mode, exp)))
end)
 in ( LrTable.NT 2, ( result, STEP1left, exp1right), rest671)
end
|  ( 3, ( ( _, ( _, STEP1left, STEP1right)) :: rest671)) => let val  
result = MlyValue.step (fn _ => (TLC.Step(NONE)))
 in ( LrTable.NT 2, ( result, STEP1left, STEP1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: ( _, ( 
MlyValue.mode mode1, _, _)) :: ( _, ( _, EVAL1left, _)) :: rest671))
 => let val  result = MlyValue.eval (fn _ => let val  (mode as mode1)
 = mode1 ()
 val  (exp as exp1) = exp1 ()
 in (TLC.Eval(SOME(mode, exp)))
end)
 in ( LrTable.NT 1, ( result, EVAL1left, exp1right), rest671)
end
|  ( 5, ( ( _, ( _, EVAL1left, EVAL1right)) :: rest671)) => let val  
result = MlyValue.eval (fn _ => (TLC.Eval(NONE)))
 in ( LrTable.NT 1, ( result, EVAL1left, EVAL1right), rest671)
end
|  ( 6, ( ( _, ( _, CHECKED1left, CHECKED1right)) :: rest671)) => let
 val  result = MlyValue.mode (fn _ => (TLC.Checked))
 in ( LrTable.NT 3, ( result, CHECKED1left, CHECKED1right), rest671)

end
|  ( 7, ( ( _, ( _, UNCHECKED1left, UNCHECKED1right)) :: rest671)) =>
 let val  result = MlyValue.mode (fn _ => (TLC.Unchecked))
 in ( LrTable.NT 3, ( result, UNCHECKED1left, UNCHECKED1right), 
rest671)
end
|  ( 8, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (T.$$(TermOps.Plus, [exp1,exp2]))
end)
 in ( LrTable.NT 4, ( result, exp1left, exp2right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (T.$$(TermOps.Times, [exp1,exp2]))
end)
 in ( LrTable.NT 4, ( result, exp1left, exp2right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (T.$$(TermOps.Cat, [exp1,exp2]))
end)
 in ( LrTable.NT 4, ( result, exp1left, exp2right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: ( _, ( _, 
LEN1left, _)) :: rest671)) => let val  result = MlyValue.exp (fn _ =>
 let val  (exp as exp1) = exp1 ()
 in (T.$$(TermOps.Len, [exp]))
end)
 in ( LrTable.NT 4, ( result, LEN1left, exp1right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.NUM NUM1, NUM1left, NUM1right)) :: rest671)
) => let val  result = MlyValue.exp (fn _ => let val  (NUM as NUM1) = 
NUM1 ()
 in (T.$$(TermOps.Num(NUM),[]))
end)
 in ( LrTable.NT 4, ( result, NUM1left, NUM1right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.STR STR1, STR1left, STR1right)) :: rest671)
) => let val  result = MlyValue.exp (fn _ => let val  (STR as STR1) = 
STR1 ()
 in (T.$$(TermOps.Str(STR),[]))
end)
 in ( LrTable.NT 4, ( result, STR1left, STR1right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.var var1, var1left, var1right)) :: rest671)
) => let val  result = MlyValue.exp (fn _ => let val  (var as var1) = 
var1 ()
 in (var)
end)
 in ( LrTable.NT 4, ( result, var1left, var1right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.letexp letexp1, letexp1left, letexp1right))
 :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  (
letexp as letexp1) = letexp1 ()
 in (letexp)
end)
 in ( LrTable.NT 4, ( result, letexp1left, letexp1right), rest671)
end
|  ( 16, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.exp exp1, _,
 _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  (exp as exp1) = exp1 ()
 in (exp)
end)
 in ( LrTable.NT 4, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.letend letend1, _, letend1right)) :: ( _, (
 MlyValue.let1 let11, _, _)) :: ( _, ( MlyValue.letstart letstart1, 
letstart1left, _)) :: rest671)) => let val  result = MlyValue.letexp
 (fn _ => let val  letstart1 = letstart1 ()
 val  (let1 as let11) = let11 ()
 val  letend1 = letend1 ()
 in (let1)
end)
 in ( LrTable.NT 6, ( result, letstart1left, letend1right), rest671)

end
|  ( 18, ( ( _, ( _, LET1left, LET1right)) :: rest671)) => let val  
result = MlyValue.letstart (fn _ => (ParserState.savetable()))
 in ( LrTable.NT 7, ( result, LET1left, LET1right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.let1 let11, _, let11right)) :: ( _, ( 
MlyValue.let2 let21, let21left, _)) :: rest671)) => let val  result = 
MlyValue.let1 (fn _ => let val  (let2 as let21) = let21 ()
 val  (let1 as let11) = let11 ()
 in ( createlet let2 let1 )
end)
 in ( LrTable.NT 8, ( result, let21left, let11right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.let2 let21, let21left, _)) :: rest671)) => let val  result = 
MlyValue.let1 (fn _ => let val  (let2 as let21) = let21 ()
 val  (exp as exp1) = exp1 ()
 in ( createlet let2 exp )
end)
 in ( LrTable.NT 8, ( result, let21left, exp1right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.IDENT IDENT1, _, _)) :: ( _, ( _, VAL1left, _)) :: rest671))
 => let val  result = MlyValue.let2 (fn _ => let val  (IDENT as IDENT1
) = IDENT1 ()
 val  (exp as exp1) = exp1 ()
 in (

                                  let
                                    val var = ParserState.addvar IDENT
                                  in
                                    (exp,var)
                                  end
                                
)
end)
 in ( LrTable.NT 9, ( result, VAL1left, exp1right), rest671)
end
|  ( 22, ( ( _, ( _, END1left, END1right)) :: rest671)) => let val  
result = MlyValue.letend (fn _ => ( ParserState.restoretable() ))
 in ( LrTable.NT 10, ( result, END1left, END1right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.IDENT IDENT1, IDENT1left, IDENT1right)) :: 
rest671)) => let val  result = MlyValue.var (fn _ => let val  (IDENT
 as IDENT1) = IDENT1 ()
 in (T.`` (ParserState.getvar IDENT))
end)
 in ( LrTable.NT 5, ( result, IDENT1left, IDENT1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.start x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : Exp_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun IDENT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.IDENT (fn () => i),p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun NUM (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.NUM (fn () => i),p1,p2))
fun STR (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.STR (fn () => i),p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun CARET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun LEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun SEMI (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun EQUALS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun NUMTYPE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun STRTYPE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun LET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun VAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun IN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun END (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun STEP (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun EVAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun CHECKED (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun UNCHECKED (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
end
end

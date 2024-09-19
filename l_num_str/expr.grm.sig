signature Exp_TOKENS =
sig
type ('a,'b) token
type svalue
val UNCHECKED:  'a * 'a -> (svalue,'a) token
val CHECKED:  'a * 'a -> (svalue,'a) token
val EVAL:  'a * 'a -> (svalue,'a) token
val STEP:  'a * 'a -> (svalue,'a) token
val END:  'a * 'a -> (svalue,'a) token
val IN:  'a * 'a -> (svalue,'a) token
val VAL:  'a * 'a -> (svalue,'a) token
val LET:  'a * 'a -> (svalue,'a) token
val STRTYPE:  'a * 'a -> (svalue,'a) token
val NUMTYPE:  'a * 'a -> (svalue,'a) token
val EQUALS:  'a * 'a -> (svalue,'a) token
val SEMI:  'a * 'a -> (svalue,'a) token
val COLON:  'a * 'a -> (svalue,'a) token
val LEN:  'a * 'a -> (svalue,'a) token
val CARET:  'a * 'a -> (svalue,'a) token
val TIMES:  'a * 'a -> (svalue,'a) token
val PLUS:  'a * 'a -> (svalue,'a) token
val STR: (string) *  'a * 'a -> (svalue,'a) token
val NUM: (int) *  'a * 'a -> (svalue,'a) token
val RPAREN:  'a * 'a -> (svalue,'a) token
val LPAREN:  'a * 'a -> (svalue,'a) token
val IDENT: (string) *  'a * 'a -> (svalue,'a) token
val EOF:  'a * 'a -> (svalue,'a) token
end
signature Exp_LRVALS=
sig
structure Tokens : Exp_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end

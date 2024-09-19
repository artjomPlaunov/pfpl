structure TermOps =
struct

  datatype t = 
    Num of int 
  | Str of string 
  | Plus | Times | Cat | Len | Let 
              
  (* ... your solution goes here ... *)
  exception NotImplemented
  fun arity e = case e of 
    Num _ => []
  | Str _ => []
  | (Plus | Times | Cat) => [0,0]
  | Len => [0]
  | Let => [0,1]

  fun equal e = case e of 
    (Num _, Num _)  => true 
  | (Str _, Str _)  => true 
  | (Plus, Plus) => true
  | (Times, Times) => true
  | (Cat, Cat) => true
  | (Len, Len) => true
  | (Let, Let) => true
  | _ => false

  fun toString (Num n) = Int.toString n
    | toString (Str s) = "\""^s^"\""
    | toString Plus = "plus"
    | toString Times = "times"
    | toString Cat = "cat"
    | toString Len = "len"
    | toString Let = "let"
end

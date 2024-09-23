functor Abt(O : OPERATOR) :> ABT where type Variable.t = Var.t 
                                 where type Operator.t = O.t
=
struct
  open List_Util

  structure Variable = Var
  structure Operator = O 

  datatype 'a view = 
    ` of Variable.t
  | \ of Variable.t * 'a
  | $ of Operator.t * 'a list

  datatype t =
    FV of Var.t
  | BV of int
  | ABS of t
  | OPER of Operator.t * t list

  exception Malformed
  exception Malformed_S of string
  exception NotImplemented

  fun map f y   = case y of 
    `v          =>  `v 
  | \(v, t)     =>  \(v, f t)
  | $(oper, es) => $(oper, List.map f es)

  fun bind (x: Var.t) (e: t) : t = 
    let fun f d e = case e of 
                      FV y => if Var.equal (x, y) then BV d else FV y
                    | BV d_ => if d <= d_ then BV (d_+1) else BV d_
                    | ABS e => f (d+1) e
                    | OPER (oper,es) => OPER (oper, (List.map (f d) es)) 
    in 
      f 0 e
    end

  fun unbind fv = 
    let fun f i e = 
      case e of 
        FV v => FV v
      | BV j => (case Int.compare (i,j) of 
                  GREATER => BV j
                | EQUAL => FV fv
                | LESS => BV (j - 1)
                )
      | ABS t => ABS (f (i+1) t)
      | OPER (oper, es) => OPER (oper, List.map (f i) es)
    in f 0 end 

  (*val foldr : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b*)

  fun arity t = case t of 
    ABS t         => 1 + (arity t)
  | _             => 0

  fun out x = case x of 
    FV y      => ` y
  | BV _      => raise Malformed_S "out BV _"
  | ABS t     => 
    let val fresh_x : Var.t = (Var.newvar "x") in
      \ (fresh_x, (unbind fresh_x) t)
    end
  | OPER (oper, es) => $ (oper, es)

  fun into x  = case x of 
    `x        => FV x 
  | \(x, e)   => ABS (bind x e)
  | $(f, es)  => 
      if O.arity f = List.map arity es
      then 
        OPER (f, es)
      else 
        raise Malformed_S "into"

  fun aequiv x = case x of 
    (FV x1, FV x2)    => if Var.equal (x1, x2) then true else false
  | (BV n1, BV n2)    => if n1 = n2 then true else false
  | (ABS t1, ABS t2)  => if aequiv (t1,t2) then true else false
  | (OPER (o1, es1), OPER (o2, es2)) => 
      if (O.equal (o1, o2)) andalso (length es1) = (length es2)
      then ListPair.all aequiv (es1, es2) else false
  | _ => false
end

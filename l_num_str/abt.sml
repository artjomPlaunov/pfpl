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

  fun unbind t = case t of
    `_        => raise Malformed
  | $_        => raise Malformed
  | \(x, e)   => 
    let val fresh_x : Var.t = Var.newvar (Var.toString x) 
    in 
      let fun f d e = case e of 
                        FV y => FV y
                      | BV d_ => (
                          case Int.compare (d, d_) of
                            GREATER => BV d_
                          | EQUAL   => FV fresh_x
                          | LESS    => BV (d_ - 1)
                      )

                      | ABS e => f (d+1) e 
                      | OPER (oper,es) => OPER (oper, (List.map (f d) es))
      in
        (fresh_x, (f 0 e)) 
      end 
    end 

  (*val foldr : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b*)

  fun arity t = case t of 
    ABS t         => 1 + (arity t)
  | OPER (_, es)  => List.foldl op+ 0 (List.map arity es)
  | _             => 0

  fun out x = case x of 
    FV y      => ` y
  | BV _      => raise Malformed
  | ABS t     => \ (unbind (out t))
  | OPER (oper, es) => $ (oper, es)

  fun into x  = case x of 
    `x        => FV x 
  | \(x, e)   => ABS (bind x e)
  | $(f, es)  => 
      if O.arity f = List.map arity es
      then 
        raise NotImplemented
      else 
        raise Malformed

  fun aequiv x = case x of 
    (FV x1, FV x2)    => if Var.equal (x1, x2) then true else false
  | (BV n1, BV n2)    => if n1 = n2 then true else false
  | (ABS t1, ABS t2)  => if aequiv (t1,t2) then true else false
  | (OPER (o1, es1), OPER (o2, es2)) => 
      if (O.equal (o1, o2)) andalso (length es1) = (length es2)
      then ListPair.all aequiv (es1, es2) else false
  | _ => false
end

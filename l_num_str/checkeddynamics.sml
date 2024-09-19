structure CheckedDynamics : DYNAMICS =
struct

  exception RuntimeError
  exception Malformed
  exception NotImplemented
  exception InfiniteRecursion

  open Term
  open TermOps

  datatype D = Step of Term.t | Val | Err

  datatype d = 
    STEP of Term.t 
  | VAL 
  | ERR
  
  fun view e        = case e of 
    STEP t  => Step t 
  | VAL     => Val
  | ERR     => Err 

  fun trystep e     = case Term.out e of 
    `_ => ERR
  | \_ => ERR
  | $(oper, es) => case (oper, es) of  
      (Num _,_) => VAL
    | (Str _,_) => VAL
    | (Plus, es) => (case List.map Term.out es of
        [$(Str _,_),_] => ERR
      | [_,$(Str _,_)] => ERR
      | [$(Num n1,_), $(Num n2,_)] => STEP (into ($(Num (n1+n2),[])))
      | [$(Num n1,[]), $(op2)] => 
          (case trystep (into ($op2)) of 
            STEP t => STEP (into ($(Plus, [into ($(Num n1,[])), t] ) ) )
          | VAL => raise InfiniteRecursion
          | ERR => ERR    
          )
      | [$(op1), $(Num n1,[])] => 
          (case trystep (into ($op1)) of 
            STEP t => STEP (into ($(Plus, [t, into ($(Num n1,[]))] ) ) )
          | VAL => raise InfiniteRecursion
          | ERR => ERR    
          )
      | _ => raise Malformed
    ) 
    | _ => raise NotImplemented

  
  
  fun eval e        = case view (trystep e) of 
    Step e  => eval e
  | Val     => e
  | Err     => raise RuntimeError
    
end

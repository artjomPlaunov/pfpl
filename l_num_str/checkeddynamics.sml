structure CheckedDynamics : DYNAMICS =
struct

  exception RuntimeError
  exception Malformed
  exception Malformed_S of string
  exception NotImplemented
  exception DeadCode

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
    | (Times, es) => (case List.map Term.out es of
        [$(Str _,_),_] => ERR 
      | [$(Num _,_),$(Str _,_)] => ERR
      | [$(Num n1,_), $(Num n2,_)] => STEP (into ($(Num (n1*n2),[])))
      | [$(Num n1,[]), $(op2)] => 
          (case trystep (into ($op2)) of 
            STEP t => STEP (into ($(Times, [into ($(Num n1,[])), t] ) ) )
          | VAL => raise DeadCode
          | ERR => ERR    
          )
      | [$(op1), $(op2)] => 
          (case trystep (into ($op1)) of 
            STEP t => STEP (into ($(Times, [t, into ($op2)])))
          | VAL => raise DeadCode
          | ERR => ERR
          ) 
      | _ => raise Malformed_S  "oper"
    ) 
    | (Plus, es) => (case List.map Term.out es of
        [$(Str _,_),_] => ERR 
      | [$(Num _,_),$(Str _,_)] => ERR
      | [$(Num n1,_), $(Num n2,_)] => STEP (into ($(Num (n1+n2),[])))
      | [$(Num n1,[]), $(op2)] => 
          (case trystep (into ($op2)) of 
            STEP t => STEP (into ($(Plus, [into ($(Num n1,[])), t] ) ) )
          | VAL => raise DeadCode
          | ERR => ERR    
          )
      | [$(op1), $(op2)] => 
          (case trystep (into ($op1)) of 
            STEP t => STEP (into ($(Plus, [t, into ($op2)])))
          | VAL => raise DeadCode
          | ERR => ERR
          ) 
      | _ => raise Malformed_S "plus"
    ) 
    | (Cat, es) => (case List.map Term.out es of
        [$(Num _,_),_] => ERR 
      | [$(Str _,_),$(Num _,_)] => ERR
      | [$(Str n1,_), $(Str n2,_)] => STEP (into ($(Str (n1^n2),[])))
      | [$(Str n1,[]), $(op2)] => 
          (case trystep (into ($op2)) of 
            STEP t => STEP (into ($(Cat, [into ($(Str n1,[])), t] ) ) )
          | VAL => raise DeadCode
          | ERR => ERR    
          )
      | [$(op1), $(op2)] => 
          (case trystep (into ($op1)) of 
            STEP t => STEP (into ($(Cat, [t, into ($op2)])))
          | VAL => raise DeadCode
          | ERR => ERR
          ) 
      | _ => raise Malformed_S "cat"
    ) 
    | (Len, es) => (case List.map Term.out es of 
        [$(Num _,_)] => ERR
      | [$(Str s,_)] => STEP (into ($(Num (String.size s),[])))
      | [$(op1)] => (case trystep (into ($op1)) of
          STEP t => STEP (into ($(Len, [t])))
        | VAL => raise DeadCode
        | ERR => ERR
        )  
      | _ => raise Malformed_S "len"
      )
    | (Let, es)  => 
    case List.map Term.out es of
			  [$(Num n1,_),\(v,t)] => 
          STEP (Term.subst (into ($(Num n1,[]))) v t) 
      | [$(Str n1,_),\(v,t)] => 
          STEP (Term.subst (into ($(Str n1,[]))) v t)
      | [$(oper),\(vt)] => 
        (case trystep (into ($(oper))) of
          STEP t => STEP (into ($(Let, [t, into (\(vt))]))) 
        | VAL => raise DeadCode
        | ERR => ERR
      )
      | _ => raise Malformed_S "(let)"
		 
		    		 
		 
  fun eval e        = case view (trystep e) of 
    Step e  => eval e
  | Val     => e
  | Err     => raise RuntimeError
    
end

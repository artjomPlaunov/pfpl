structure TypeChecker : TYPECHECKER =
struct
  exception TypeError
  exception NotImplemented

  structure T = Term
  structure Ty = Type
  structure Tb = Context
  structure TO = TypeOps
  structure TmO = TermOps
		
  type context = Type.t Context.map

  fun checktype ctx t  = case Term.out t of

	T.` v  =>
		(case (Tb.find (ctx,v)) of
		    NONE => raise TypeError
		  | SOME ty => (case (Ty.out ty) of 
				    (Ty.`_ | Ty.\_) => raise TypeError
				  | Ty.$(TO.NUM,_) => (Ty.into (Ty.$(TO.NUM,[])))
				  | Ty.$(TO.STR,_) => (Ty.into (Ty.$(TO.STR,[])))
			       )
		)
    | T.\_=> raise NotImplemented  
    | T.$(TmO.Num _,_) => (Ty.into (Ty.$(TO.NUM,[])))
    | T.$(TmO.Str _,_) => (Ty.into (Ty.$(TO.STR,[])))
    | T.$(TmO.Plus , es) =>
      (case es of
	   [t1,t2] =>
	   let val ty1 = checktype ctx t1
	       val ty2 = checktype ctx t2 in
	       (case (Ty.out ty1,Ty.out ty2) of
		    (Ty.$(TO.NUM,[]),Ty.$(TO.NUM,[])) => (Ty.into (Ty.$(TO.NUM,[])))
		 | _ => raise TypeError
	       )
	   end
	       
	| _ => raise NotImplemented
      
      )
    | T.$(TmO.Times, es) =>
      (case es of
	   [t1,t2] =>
	   let val ty1 = checktype ctx t1
	       val ty2 = checktype ctx t2 in
	       (case (Ty.out ty1,Ty.out ty2) of
		    (Ty.$(TO.NUM,[]),Ty.$(TO.NUM,[])) => (Ty.into (Ty.$(TO.NUM,[])))
		 | _ => raise TypeError
	       )
	   end
	       
	| _ => raise NotImplemented
      
      )
    | T.$(TmO.Cat, es) =>
      (case es of
	   [t1,t2] =>
	   let val ty1 = checktype ctx t1
	       val ty2 = checktype ctx t2 in
	       (case (Ty.out ty1,Ty.out ty2) of
		    (Ty.$(TO.STR,[]),Ty.$(TO.STR,[])) => (Ty.into (Ty.$(TO.STR,[])))
		 | _ => raise TypeError
	       )
	   end
	| _ => raise TypeError
      
      )
    | T.$(TmO.Len, es) =>
      (case es of
	   [t1] =>
	   let val ty1 = checktype ctx t1 in
	       (case Ty.out ty1 of
		    Ty.$(TO.STR,[]) => Ty.into (Ty.$(TO.NUM,[]))
		 | _ => raise TypeError
	       )
	   end
	| _ => raise TypeError
      )
    | T.$(TmO.Let, es) =>
      (case es of
	   [e, abs] =>
	   let val ty_e = checktype ctx e in
	       case T.out abs of
		   T.\(v,t) =>
		   let val new_ctx = Tb.insert (ctx,v,ty_e) in
		       let val ty_abs = checktype new_ctx t in
			   ty_abs
		       end
		   end
		| _ => raise TypeError
	       
	   end
	| _ => raise TypeError
      )
end

functor ABT_Util(A : ABT) : ABT_UTIL = 
struct
  open A 
  exception NotImplemented

  fun uncurry f a b = f (a, b)

  fun `` e           = into (`e)
  fun \\ e           = into (\e) 
  fun $$ e           = into ($e) 


  fun dedup [] = []
    | dedup (x :: xs) =
      if List.exists (fn y => Variable.equal (x,y)) xs
      then dedup xs
      else x :: dedup xs

  fun freevars e = 
    let fun acc bound res e = 
      case out e of 
        `v => if  List.exists (uncurry Variable.equal v) bound orelse 
                  List.exists (uncurry Variable.equal v) res  
              then res else v::res 
      | \(v, t) => acc (v::bound) res t
      | $(oper, es) => 
          dedup (List.concat (List.map (acc bound res ) es))
    in 
      acc [] [] e 
    end 

  (* [e1/x]e2 *)
  fun subst e1 x e2    = case out e2 of 
    `v        =>  if Variable.equal (x,v) then e1 else ``v
  | \(v,t)    =>  if Variable.equal (x,v) 
                  then \\(v,t) else \\(v, subst e1 x t)
  | $(oper, es) => $$(oper, List.map (subst e1 x) es)
   
  fun toString e     = raise NotImplemented 
end

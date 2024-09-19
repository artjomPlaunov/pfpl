structure TypeChecker : TYPECHECKER =
struct
  exception TypeError
  exception NotImplemented

  type context = Type.t Context.table

  (* ... your solution goes here ... *)
   fun checktype ctx t  = raise NotImplemented
end

signature TYPECHECKER =
sig
  exception TypeError

  type context = Type.t Context.table

  val checktype : context -> Term.t -> Type.t
end

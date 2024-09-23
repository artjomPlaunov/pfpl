signature TYPECHECKER =
sig
  exception TypeError

  type context = Type.t Context.map

  val checktype : context -> Term.t -> Type.t
end

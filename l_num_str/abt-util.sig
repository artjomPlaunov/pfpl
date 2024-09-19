signature ABT_UTIL =
sig
  include ABT

  val freevars : t -> Variable.t list 
  (* [e1/x]e2 *)
  val subst    : t -> Variable.t -> t -> t

  val `` : Variable.t -> t
  val \\ : Variable.t * t -> t
  val $$ : Operator.t * t list -> t

  val toString : t -> string
end

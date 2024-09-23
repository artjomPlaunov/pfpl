signature DYNAMICS =
sig
  type d
  
  datatype D = Step of Term.t | Val | Err
  val view : d -> D

  exception Malformed
  exception Malformed_S of string

  val trystep : Term.t -> d
  
  exception RuntimeError

  val eval : Term.t -> Term.t
end

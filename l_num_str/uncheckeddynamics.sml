structure UncheckedDynamics : DYNAMICS =
struct

  exception RuntimeError
  exception Malformed
  exception NotImplemented

  datatype D = Step of Term.t | Val | Err

  (* ... your solution goes here ... *)
  datatype d = ToBeImplemented
     (* One datatype branch is needed for if the expression steps... *)
     (* ...another is needed for if it is a value... *)
     (* NO NEED for a branch for error (possibly excluded by the typesystem) *)
  
  fun view e        = raise NotImplemented
  fun trystep e     = raise NotImplemented
  fun eval e        = raise NotImplemented
end

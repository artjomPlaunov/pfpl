signature TESTHARNESS =
sig
  val runtests : bool -> unit
  (*val runspectests : bool -> unit*)
  val runvartests : bool -> unit

  val runalltests : bool -> unit
end

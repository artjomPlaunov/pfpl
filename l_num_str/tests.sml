structure TestHarness :> TESTHARNESS =
struct
	structure T = TypeChecker
	structure UD = UncheckedDynamics
	structure CD = CheckedDynamics
	structure TLC = TopLevelCommands
	structure TL = TopLevel

	datatype 'a expected_result = Pass of 'a | Fail
  		| TypeFail | ParseFail | EvalFail

	fun vprint v s = if v then print s else ()

	fun success v n = (if v then print ((Int.toString n) ^ ": Success!\n")
		else () ; true)
	
	fun hdl verb n f comp exp = 
	(let
		val res = f()
	in
		(case exp of
			Pass(x) => 
			let
				val passed = comp(res,x)
			in
				if passed then success verb n 
				else (vprint verb ((Int.toString n) 
					^ ": Failed: Result does not match\n") ; false)
			end
		|	_ => ((vprint verb "Failed...\n") ; false))
	end) 
		handle (ParserState.Parse(s)) => 
			(case exp of
				Fail => success verb n
			|	ParseFail => success verb n
			|	_ => (vprint verb ((Int.toString n) ^ ": Failed: Parse Error: "
					^ s ^ "\n");
					false))
		|	T.TypeError => 
			(case exp of
				Fail => success verb n
			|	TypeFail => success verb n
			|	_ => (vprint verb ((Int.toString n) ^ ": Failed: Type Error\n");
					false))
		|	UD.RuntimeError => 
			(case exp of
				Fail => success verb n
			|	EvalFail => success verb n
			|	_ => (vprint verb ((Int.toString n) ^ 
					": Failed: Runtime Error\n") ; false))
		|	CD.RuntimeError => 
			(case exp of
				Fail => success verb n
			|	EvalFail => success verb n
			|	_ => (vprint verb ((Int.toString n) ^ 
					": Failed: Runtime Error\n") ; false))
		|	UD.Malformed => 
			(case exp of
				Fail => success verb n
			|	_ => (vprint verb ((Int.toString n) ^ 
					": Failed: Unexpected Error in UncheckedDynamics\n") ; false))
		|	CD.Malformed => 
			(case exp of
				Fail => success verb n
			|	_ => (vprint verb ((Int.toString n) ^ 
					": Failed: Unexpected Error in CheckedDynamics\n") ; false))
		|	_ => 
			(case exp of
				Fail => success verb n
			|	_ => (vprint verb ((Int.toString n) ^ 
					": Failed: Other Exception\n") ; false))
  

    exception TestError
    fun toNumVal t = (fn Term.$((TermOps.Num n),_) => n | _ => raise TestError) (Term.out t);
    fun toStrVal t = (fn Term.$((TermOps.Str s),_) => s | _ => raise TestError) (Term.out t);
  
	(*Each eval evaluates to either a number or a string. A test takes an 
    eval command and an expected result of the corresponding type. *)
    datatype test = Num of string * (int expected_result) | Str of string * (string expected_result)
	val tests = 
	[
        Num("eval checked 45 + 7", Pass(52)),
        Num("eval checked 5 * 7", Pass(35)),
		Num("eval unchecked len(\"bingo\")", Pass(5)),
		Num("eval checked let val x = 24 in x + 4 end", Pass(28)),   
		Num("eval unchecked \"abc\"+5", Fail),
		Str("eval checked \"\"", Pass("")),
		Str("eval checked \"18\"^\"312\"", Pass("18312"))
    ]

	fun runtest v (Num(text,exp_res),(L,n)) = ((hdl v n (fn () => TL.eval text) 
			(fn (TLC.Val(t),x) => (toNumVal t = x) | _ => raise TestError) exp_res)::L,n+1)
	  | runtest v (Str(text,exp_res),(L,n)) = ((hdl v n (fn () => TL.eval text) 
			(fn (TLC.Val(t),x) => (toStrVal t = x) | _ => raise TestError) exp_res)::L,n+1)
	
    (*Each step command either steps to another term, is a val
      or results in an error. A steptest tests whether a command steps or not.
      In the case of unchecked dynamics, an ill typed expression is expected
      to result in a TypeFail.*)
    type steptest = string * (bool expected_result)
	val steptests = 
	[
        ("step checked 45 + 8", Pass(true)),
        ("step checked 8", Pass(false)),
		("step checked 8 + \"bingo\"", Pass(false)),
		("step checked 8 * \"bingo\"", Pass(false)),
		("step checked (45 + 8) + \"bingo\"", Pass(true)),
		("step checked (45 * 8) * \"bingo\"", Pass(true)),
		("step unchecked (45 + 8) + \"bingo\"", TypeFail)
    ]

	fun runsteptest v ((text,exp_res),(L,n)) = ((hdl v n (fn () => TL.eval text) 
			(fn (TLC.Next(t),x) => x | (_,x) => not(x)) exp_res)::L,n+1)


	(* Each spectest takes an initial program, a function that 
	can transform ABTs, and an expected result. In addition, the
	initial program takes a list of free variables (as strings) 
	that can occur in the body of the program. The test script
	parses the input with respect to the given free variables,
	runs the given funtion on the result, and tests alpha equivalence
	with the result from parsing the expected result with respect
	to its own list of free variables.
	
	This is a good opportunity to test things that just running
	programs won't necessarily test, such as aequiv and subst. *)

	(*type spectest = (string list * string) * (Term.t -> Term.t) 
			* (string list * string expected_result)
	val spectests = []
     
	fun runspectest v ((orig,f,exp),(L,n)) = ((hdl v n
			(fn () => f (TL.parse_vars orig)) 
			(fn (l,r) => Term.aequiv(l,TL.parse_vars r)) exp)::L,n+1)*)

	(* Each vartest takes an ABT and an expected_result that represents the
	number of free variables that should be found in that expression or an
	expected failure.

	The test passes if the number of free variables returned by freevars
	is the same as the number provided or if the expected error is raised. *)

    val varx = Var.newvar "x";
    val vary = Var.newvar "y";
    val varz = Var.newvar "z";
    val numcon = fn n => Term.$$(TermOps.Num n, [])
    val strcon = fn s => Term.$$(TermOps.Str s, [])


	type vartest = Term.t * (int expected_result)
	val vartests = 
    [
            (Term.``(varx), Pass(1)),
            (Term.$$(TermOps.Plus,[Term.``(varx), Term.``(vary)]), Pass(2)),
            (Term.$$(TermOps.Let, [numcon 5, Term.\\(varx, Term.$$(TermOps.Plus,[Term.``(varx), numcon 2]))]), Pass(0)),
            (Term.$$(TermOps.Let, [
                        Term.``(varx), 
                        Term.\\(varx, Term.$$(TermOps.Plus,[Term.``(varx), numcon 2]))]), 
            Pass(1))
    ]


	fun runvartest v ((abt,exp),(L,n)) = ((hdl v n 
			(fn () => List.length (Term.freevars abt)) (op =) exp)::L,n+1)
      
	fun summarize [] (pass,fail) = 
		((if fail > 0 then
        TextIO.print "-------------------------------------------------------\n"
		else ());
       TextIO.print ("\nTests completed: " 
	   		^ (Int.toString (pass + fail)) ^ "\n");
       TextIO.print ("Tests passed   : " ^ (Int.toString pass)        ^ "\n");
       TextIO.print ("Tests failed   : " ^ (Int.toString fail)        ^ "\n");
       if fail = 0 then
         TextIO.print "Congratulations!\n" 
       else
         () )
	|	summarize (result::results) (pass,fail) =
        let 
          val stats' = if result then (pass+1,fail) else (pass,fail+1)
        in
          summarize results stats'
        end

	fun fst (x,_) = x (* makes the next three functions much nicer *)
                 
	fun runtests verbose = (print "\n\nRunning normal tests...\n";
  		summarize (fst (foldl (runtest verbose) ([],1) tests)) (0,0))
	fun runsteptests verbose = (print "\n\nRunning step tests...\n";
  		summarize (fst (foldl (runsteptest verbose) ([],1) steptests)) (0,0))
	(*fun runspectests verbose = (print "\n\nRunning special tests...\n";
  		summarize (fst (foldl (runspectest verbose) ([],1) spectests)) (0,0))*)
	fun runvartests verbose = (print "\n\nRunning freevar tests...\n";
  		summarize (fst (foldl (runvartest verbose) ([],1) vartests)) (0,0))
           
	fun runalltests v = (runtests v ; runsteptests v; (*runspectests v ;*) runvartests v)
end

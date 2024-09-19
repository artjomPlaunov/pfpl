signature ENVIRONMENT =
sig
	type env

	val symbolTable : env -> Var.t symbols.map
	val dynamicContext : env -> real Context.map
	val staticContext : env -> Type.t Context.map

	val toTuple : env -> Var.t symbols.map * 
			real Context.map * Type.t Context.map
	val fromTuple : Var.t symbols.map * real Context.map 
			* Type.t Context.map -> env
	
	val updateEnv : (string * Type.t * real) * env -> env
	val updateEnvVar : string -> Type.t -> real -> env -> env * Var.t

	val newenv : unit -> env
end

structure Environment :> ENVIRONMENT =
struct
	datatype env = Env of 
		Var.t symbols.map *
		real Context.map *
		Type.t Context.map
	
	fun symbolTable (Env(st,_,_)) = st
	fun dynamicContext (Env(_,dc,_)) = dc
	fun staticContext (Env(_,_,sc)) = sc

	fun toTuple (Env e) = e
	fun fromTuple e = Env e
	
	fun updateEnvVar id t r (Env(st,dc,sc)) = 
	let
		val var = Var.newvar id
	in
		(Env(symbols.insert(st,id,var),
			Context.insert(dc,var,r),
			Context.insert(sc,var,t)),
		var)
	end

	fun updateEnv ((id,t,r),e) = 
		let val (env,_) = updateEnvVar id t r e in env end

	fun newenv() = 
		   foldr updateEnv (Env(symbols.empty,Context.empty,Context.empty))
			  [(BaseTypes.M,Type.$$(TypeOps.M,[]),1.0),
			   (BaseTypes.L,Type.$$(TypeOps.L,[]),1.0),
		    	   (BaseTypes.T,Type.$$(TypeOps.T,[]),1.0)]

end

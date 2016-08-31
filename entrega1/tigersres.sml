structure tigersres =
struct

open tigerabs
open tigertab
open tigertips

datatype EnvEntry =
	VIntro	(* int readonly *)
	| Var of {ty: Tipo}
	| Func of {
    	level: unit,
    	label: tigertemp.label, (* string *)
    	formals: Tipo list,
    	result: Tipo,
    	extern: bool (* Si es fun de librería *)
  		}

val mainLevel = ()
end

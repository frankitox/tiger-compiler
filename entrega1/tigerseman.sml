structure tigerseman :> tigerseman =
struct

open tigerabs
open tigersres

type expty = {exp: unit, ty: Tipo}

type venv = (string, EnvEntry) tigertab.Tabla
type tenv = (string, Tipo) tigertab.Tabla

val tab_tipos : (string, Tipo) Tabla = tabInserList(
	tabNueva(),
	[("int", TInt), ("string", TString)])

val tab_vars : (string, EnvEntry) Tabla = tabInserList(
	tabNueva(),
	[("print", Func{level=mainLevel, label="print",
		formals=[TString], result=TUnit, extern=true}),
	("flush", Func{level=mainLevel, label="flush",
		formals=[], result=TUnit, extern=true}),
	("getchar", Func{level=mainLevel, label="getstr",
		formals=[], result=TString, extern=true}),
	("ord", Func{level=mainLevel, label="ord",
		formals=[TString], result=TInt, extern=true}),
	("chr", Func{level=mainLevel, label="chr",
		formals=[TInt], result=TString, extern=true}),
	("size", Func{level=mainLevel, label="size",
		formals=[TString], result=TInt, extern=true}),
	("substring", Func{level=mainLevel, label="substring",
		formals=[TString, TInt, TInt], result=TString, extern=true}),
	("concat", Func{level=mainLevel, label="concat",
		formals=[TString, TString], result=TString, extern=true}),
	("not", Func{level=mainLevel, label="not",
		formals=[TInt], result=TInt, extern=true}),
	("exit", Func{level=mainLevel, label="exit",
		formals=[TInt], result=TUnit, extern=true})
	])

fun tipoReal (TTipo s, (env : tenv)) : Tipo = 
    (case tabBusca(s , env) of 
         NONE => raise Fail "tipoReal Ttipo"
       | SOME t => t)
  | tipoReal (t, _) = t

fun tiposIguales (TRecord _) TNil = true
  | tiposIguales TNil (TRecord _) = true 
  | tiposIguales (TRecord (_, u1)) (TRecord (_, u2 )) = (u1=u2)
  | tiposIguales (TArray (_, u1)) (TArray (_, u2)) = (u1=u2)
  | tiposIguales (TTipo _) b = raise Fail "No debería pasar! (1)"
		(* let *)
		(* 	val a = case !r of *)
		(* 		SOME t => t *)
		(* 		| NONE => raise Fail "No debería pasar! (1)" *)
		(* in *)
		(* 	tiposIguales a b *)
		(* end *)
  | tiposIguales a (TTipo _) = raise Fail "No debería pasar! (2)"
		(* let *)
		(* 	val b = case !r of *)
		(* 		SOME t => t *)
		(* 		| NONE => raise Fail "No debería pasar! (2)" *)
		(* in *)
		(* 	tiposIguales a b *)
		(* end *)
  | tiposIguales a b = (a=b)


val listAnd = foldl (fn (t1, t2) => t1 andalso t2) true

(* val transExp : venv * tenv -> expty *)
fun transExp(venv, tenv) =
	let 
		fun error(s, p) = raise Fail ("Error -- línea "^Int.toString(p)^": "^s^"\n")
		fun trexp(VarExp v) = trvar(v)
		| trexp(UnitExp _) = {exp=(), ty=TUnit}
		| trexp(NilExp _)= {exp=(), ty=TNil}
		| trexp(IntExp(i, _)) = {exp=(), ty=TInt}
		| trexp(StringExp(s, _)) = {exp=(), ty=TString}
		| trexp(CallExp({func, args}, nl)) = (
			case tabBusca(func,venv) of
				SOME (Func{formals=form, result=res, ...}) =>
					let
						val targs  = map (fn (ex) => #ty(trexp ex)) args
						val zipped = ListPair.zip(form, targs)
						val bools  = map (fn (t1, t2) => tiposIguales t1 t2) zipped
					in
						if listAnd bools then
							{exp=(), ty=res}
						else
							error("Error los tipos de los argumentos no coinciden (trexp CallExp)", nl)
					end	
			 	| SOME _ => error("Error func no es de tipo Func (trexp CallExp)", nl)
				| NONE   => error("Error no existe la funcion func (trexp CallExp)", nl)
			)
		| trexp(OpExp({left, oper=EqOp, right}, nl)) =
			let
				val {exp=_, ty=tyl} = trexp left
				val {exp=_, ty=tyr} = trexp right
			in
				if tiposIguales tyl tyr andalso not (tyl=TNil andalso tyr=TNil) andalso tyl<>TUnit then {exp=(), ty=TInt}
					else error("Tipos no comparables", nl)
			end
		| trexp(OpExp({left, oper=NeqOp, right}, nl)) = 
			let
				val {exp=_, ty=tyl} = trexp left
				val {exp=_, ty=tyr} = trexp right
			in
				if tiposIguales tyl tyr andalso not (tyl=TNil andalso tyr=TNil) andalso tyl<>TUnit then {exp=(), ty=TInt}
					else error("Tipos no comparables", nl)
			end
		| trexp(OpExp({left, oper, right}, nl)) = 
			let
				val {exp=_, ty=tyl} = trexp left
				val {exp=_, ty=tyr} = trexp right
			in
				if tiposIguales tyl tyr then
					case oper of
						PlusOp => if tipoReal(tyl, tenv)=TInt then {exp=(),ty=TInt} else error("Error de tipos", nl)
						| MinusOp => if tipoReal(tyl,tenv)=TInt then {exp=(),ty=TInt} else error("Error de tipos", nl)
						| TimesOp => if tipoReal(tyl,tenv)=TInt then {exp=(),ty=TInt} else error("Error de tipos", nl)
						| DivideOp => if tipoReal(tyl,tenv)=TInt then {exp=(),ty=TInt} else error("Error de tipos", nl)
						| LtOp => if tipoReal(tyl,tenv)=TInt orelse tipoReal(tyl,tenv)=TString then {exp=(),ty=TInt} else error("Error de tipos", nl)
						| LeOp => if tipoReal(tyl,tenv)=TInt orelse tipoReal(tyl,tenv)=TString then {exp=(),ty=TInt} else error("Error de tipos", nl)
						| GtOp => if tipoReal(tyl,tenv)=TInt orelse tipoReal(tyl,tenv)=TString then {exp=(),ty=TInt} else error("Error de tipos", nl)
						| GeOp => if tipoReal(tyl,tenv)=TInt orelse tipoReal(tyl,tenv)=TString then {exp=(),ty=TInt} else error("Error de tipos", nl)
						| _ => raise Fail "No debería pasar! (3)"
				else error("Error de tipos", nl)
			end
		| trexp(RecordExp({fields, typ}, nl)) =
			let
				(* Traducir cada expresión de fields *)
				val tfields = map (fn (sy,ex) => (sy, trexp ex)) fields

				(* Buscar el tipo *)
				val (tyr, cs) = case tabBusca(typ, tenv) of
					SOME t => (case tipoReal(t,tenv) of
						TRecord (cs, u) => (TRecord (cs, u), cs)
						| _ => error(typ^" no es de tipo record", nl))
					| NONE => error("Tipo inexistente ("^typ^")", nl)
				
				(* Verificar que cada campo esté en orden y tenga una expresión del tipo que corresponde *)
				fun verificar [] [] = ()
				  | verificar (c::cs) [] = error("Faltan campos", nl)
				  | verificar [] (c::cs) = error("Sobran campos", nl)
				  | verificar ((s,t,_)::cs) ((sy,{exp,ty})::ds) =
						if s<>sy then error("Error de campo", nl)
						else if tiposIguales ty (!t) then verificar cs ds
							 else error("Error de tipo del campo "^s, nl)
				val _ = verificar cs tfields
			in
				{exp=(), ty=tyr}
			end
		| trexp(SeqExp(s, nl)) =
			let	val lexti = map trexp s
				val exprs = map (fn{exp, ty} => exp) lexti
				val {exp, ty=tipo} = hd(rev lexti)
			in	{ exp=(), ty=tipo } end
		| trexp(AssignExp({var = SimpleVar s, exp}, nl)) =
			let 
				val t1 = tabSaca(s,tab_tipos)
				val t2 = #ty(trexp exp)	
			in 
				if tiposIguales t1 t2 then {exp=(), ty=TUnit} 
									  else error("Error de tipo en simplevar", nl)
			end
			(*VER CASO DE ERROR CUANDO LA VARIABLE NO ESTA DECLARADA*)
		| trexp(AssignExp({var, exp}, nl)) =
			let 
				val t1 = #ty(trvar (var,nl) )
				val t2 = #ty(trexp exp)	
			in 
				if tiposIguales t1 t2 then {exp=(), ty=TUnit} 
									  else error("Error de tipo en asignacion", nl)
			end
		| trexp(IfExp({test, then', else'=SOME else'}, nl)) =
			let val {exp=testexp, ty=tytest} = trexp test
			    val {exp=thenexp, ty=tythen} = trexp then'
			    val {exp=elseexp, ty=tyelse} = trexp else'
			in
				if tipoReal(tytest,tenv)=TInt andalso tiposIguales tythen tyelse then {exp=(), ty=tythen}
				else error("Error de tipos en if" ,nl)
			end
		| trexp(IfExp({test, then', else'=NONE}, nl)) =
			let val {exp=exptest,ty=tytest} = trexp test
			    val {exp=expthen,ty=tythen} = trexp then'
			in
				if tipoReal(tytest,tenv)=TInt andalso tythen=TUnit then {exp=(), ty=TUnit}
				else error("Error de tipos en if", nl)
			end
		| trexp(WhileExp({test, body}, nl)) =
			let
				val ttest = trexp test
				val tbody = trexp body
			in
				if tipoReal(#ty ttest, tenv) = TInt andalso #ty tbody = TUnit 
					then {exp=(), ty=TUnit}
					else if tipoReal(#ty ttest, tenv) <> TInt 	
							then error("Error de tipo en la condición", nl)
							else error("El cuerpo de un while no puede devolver un valor", nl)
			end
		| trexp(ForExp({var, escape, lo, hi, body}, nl)) =
			let 
				val tlo   = #ty(trexp lo)
				val thi   = #ty(trexp hi)
				val venv2 = tabInserta(var, VIntro, venv)
				val tbody = #ty( transExp(venv2, tenv) body )
			in 
				if tiposIguales tlo TInt andalso tiposIguales thi TInt andalso tiposIguales tbody TUnit 
						then {exp=(), ty=TUnit} 
						else if tiposIguales tlo TInt andalso tiposIguales thi TInt
							then error("El cuerpo de un for no puede devolver un valor", nl)
							else if tiposIguales tlo TInt
								then error("Error hi no es de tipo int", nl)
								else error("Error lo no es de tipo int", nl)
			end
		| trexp(LetExp({decs, body}, _)) =
			let	val (venv', tenv', _) = List.foldl (fn (d, (v, t, _)) => trdec(v, t) d) (venv, tenv, []) decs
				val {exp=expbody,ty=tybody}=transExp (venv', tenv') body
			in 	{exp=(), ty=tybody}	end
		| trexp(BreakExp nl) = {exp=(), ty=TUnit} (*COMPLETAR PERO MAS ADELANTE*)
		| trexp(ArrayExp({typ, size, init}, nl)) =
			(*  (ArrayExp  of {typ: symbol, size: exp, init: exp} * pos)
				let type A = array of int
				var a:=A[100] of 1 		*)
			case tabBusca(typ, tenv) of 
				SOME (TArray(tref,u)) =>
					if tiposIguales (#ty(trexp size)) TInt then
						if tiposIguales (#ty(trexp init)) (!tref) then 
							{exp=(), ty= !tref}
						else error("Error init no es de tipo compatible (trexp ArrayExp)", nl)
					else error("Error size no es de tipo TInt o igual (trexp ArrayExp)", nl)
			 	| SOME _ => error("Error typ no es de tipo TArray", nl)
				| NONE   => error("Error no existe el tipo typ (trexp ArrayExp)", nl)

		and trvar(SimpleVar s, nl) = 
			(*aca tengo que fijarme si esta definida y que tipo tiene.
			tengo que devolver el tipo*)			
			(case tabSaca(s, venv) of
			Var t => {exp=(), ty= #ty(t) }
			| _   => error("no definida la variable en trvar(SimpleVar....)", nl)
			)
		| trvar(FieldVar(v, s), nl) = (
			case #ty(trvar(v,nl)) of
			TRecord (cs, u) => (
				(* cs tiene tipo (string * Tipo ref * int) list *)
					case (List.find (fn (str,typ,i) => s=str) cs) of
					SOME (str,typ,i) => {exp=(), ty= !typ }
					| NONE 			 => error("s no esta en el record v (trvar FieldVar)", nl)
				)

			| _				=> error("FieldVar v no es un TRecord", nl) 
			)
		| trvar(SubscriptVar(v, e), nl) =
			case #ty(trvar(v,nl)) of 
				TArray (rt,u) => (
				  case #ty(trexp(e)) of
				    TInt => {exp=(), ty= !rt }
				    | _  => error("e no es de tipo int (trvar SubscriptVar)", nl) )
				| _           => error("v no es de tipo array (trvar SubscriptVar)", nl)

(*
datatype EnvEntry = 
	VIntro	(* int readonly *)
	| Var of {ty: Tipo}
	| Func of { level: unit, label: tigertemp.label (*string*),	
				formals: Tipo list, result: Tipo, 
				extern: bool 	}
and dec =
    FunctionDec of ({name: symbol, params: field list, result: symbol option, body: exp} * pos) list
  | VarDec      of {name: symbol, escape: bool ref, typ: symbol option, init: exp} * pos
  | TypeDec     of ({name: symbol, ty: ty} * pos) list

field = {name: symbol, escape: bool ref, typ: ty} *)

(* transformarFunctionDecEnEnvEntry ({name = n,params = ps, result = r, body = b}) =
	let fs = map (fn (f) => #typ(f)) ps (*ver como pasar de ty a Tipo*)
		case r of
			SOME s => val res = tabSaca(s,tenv)
			| NONE => val res = TUnit
	in Fun({level = (), label = n, formals = fs, result = res, extern = False})
*)
		and trdec (venv, tenv) (VarDec ({name,escape,typ=NONE,init},pos)) = 
			(* 	var id := exp (en este caso tengo que asignar el tipo a id)  
			var r := nil (tiene que dar error) *)
			let val t    = #ty(trexp init)
			in  
				if tiposIguales t TNil then 
					error("No se puede asignar NIL trdec VarDec.1", pos)
				else
					(tabRInserta(name, Var({ty = t}), venv),
					 tenv, [])
			end
		| trdec (venv,tenv) (VarDec ({name,escape,typ=SOME s,init},pos)) =
			(* 	(var id:id:=exp) en este caso tengo que chequearel tipo de id con el de exp
				type R = {}
				var r:R:=nil (deberia andar) *)
			let 
				val texp  = #ty(trexp init)
				val tdado = tabSaca(s,tenv)
			in  
				if tiposIguales texp tdado then 
					( tabRInserta(name, Var({ty = texp}), venv), tenv, [])
				else
					error("Error de tipos. No coinciden (trdec VarDec.2)", pos)
			end
		| trdec (venv,tenv) (FunctionDec funcs) = 
      let
        fun mapParam ({name=n, escape=boolref, typ=ty}) =
          tabSaca tenv ty
          handle noExiste => print ("[trdec, FunctionDec] El tipo del parámetro " ^
                                    n ^ " no existe, línea: " ^ p)
        fun toFuncEnvEntry ({name=n, params=p, result=r, body=_}, p) =
          Func { level   = (),
                 label   = n,
                 formals = map mapParam p,
                 result  = if r = NONE then TUnit else tabSaca tenv (valOf r),
                 extern  = false }
          handle noExiste => print "[trdec, FunctionDec] El tipo de retorno no existe, línea: " ^ p
(*			let 
				envEntrys = map transformarEnEnvEntry fs
				venv2 = map tabRInserta () 
*)
			(venv, tenv, []) (*COMPLETAR*)
		| trdec (venv,tenv) (TypeDec ts) =
			(venv, tenv, []) (*COMPLETAR*)
	in trexp end

(* transProg: exp -> unit *)
fun transProg ex =
	let	val main =
				LetExp({decs=[FunctionDec[({name="_tigermain", params=[],
                                    result=NONE,       body=ex}, 0)]],
						body=UnitExp 0}, 0)
		(* Cuando se termine el testeo cambiar ex por main. *)
		val _ = transExp(tab_vars, tab_tipos) ex
	in	print "bien!\n" end
end

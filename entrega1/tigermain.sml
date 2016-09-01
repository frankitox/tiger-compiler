open tigerlex
open tigergrm
open tigerescap
open tigerseman
open BasicIO Nonstdio

(* el tipo instream es para archivos abiertos *)
fun lexstream(is: instream) =
	Lexing.createLexer(fn b => fn n => buff_input is b 0 n);

fun errParsing(lbuf) = (print("Error en parsing!("
	^(makestring(!num_linea))^
	")["^(Lexing.getLexeme lbuf)^"]\n"); raise Fail "fin!")

(* main: string list -> unit? *)
fun main(args) =
	let
		fun arg(l, s) =
			(* List.exists : ('a -> bool) -> 'a list -> bool *)
			(List.exists (fn x => x=s) l, List.filter (fn x => x<>s) l)
		(* define flags y filtra args *)
		val (arbol, l1)   = arg(args, "-arbol")
		val (escapes, l2) = arg(l1, "-escapes")
		val (ir, l3)      = arg(l2, "-ir")
		val (canon, l4)   = arg(l3, "-canon")
		val (code, l5)    = arg(l4, "-code")
		val (flow, l6)    = arg(l5, "-flow")
		val (inter, l7)   = arg(l6, "-inter")
		val entrada = (* entrada : instream *)
			case l7 of
			[n] => ((open_in n)
					handle _ => raise Fail (n^" no existe!"))
			| [] => std_in
			| _ => raise Fail "opción dsconocida!"
		val lexbuf = lexstream entrada
		val expr = prog Tok lexbuf handle _ => errParsing lexbuf (*en AST*)
		val _  = findEscape(expr) (*no nos interesa el resultado*)
		val _ = if arbol then tigerpp.exprAst expr else ()
	in
		transProg(expr); (*definida en tigersemant*)
		print "yes!!\n"
	end	handle Fail s => print("Fail: "^s^"\n")

val _ = main(CommandLine.arguments())

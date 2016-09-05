structure getAST :> getAST =
struct

open tigerlex
open tigergrm
open tigerabs
open BasicIO Nonstdio

(* el tipo instream es para archivos abiertos *)
fun lexstream(is: instream) =
  Lexing.createLexer(fn b => fn n => buff_input is b 0 n);

fun errParsing(lbuf) = (print("Error en parsing!("
  ^(makestring(!num_linea))^
  ")["^(Lexing.getLexeme lbuf)^"]\n"); raise Fail "fin!")

fun getAST(file) =
  let
    val entrada = ((open_in file)
       handle _ => raise Fail (file^" no existe!"))
    val lexbuf  = lexstream entrada
  in
    prog Tok lexbuf
      handle _ => errParsing lexbuf
  end
end

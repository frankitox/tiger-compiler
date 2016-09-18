structure ejercicio7 :> ejercicio7 =
struct

open tigersres;
open PP;

 (* ppstrm: ppstream *)
val ppstrm = PP.mk_ppstream {
  consumer  =
    fn s => TextIO.output(TextIO.stdOut, s),
  linewidth = 79,
  flush =
    fn() => TextIO.flushOut TextIO.stdOut
}

(* Wrapper for `add_string ppstrm`
    add_str: string -> unit *)
fun add_str str = add_string ppstrm str

 (* ppEnvEntry': EnvEntry -> unit *)
fun ppEnvEntry' VIntro =
      add_str "VIntro"
|   ppEnvEntry' (Var {ty=tipo}) = (
      add_str "Var {";
      add_str "asd";
      add_str "}" )
|   ppEnvEntry' (Func record) = (
      add_str "Func {";
      add_str "HOLA";
      add_str "}" )
 (* ppTipo: Tipo -> unit *)
and ppTipo TUnit =
      add_str "unit"
|   ppTipo TNil =
      add_str "nil"
|   ppTipo TInt =
      add_str "int"
|   ppTipo TString =
      add_str "string"
|   ppTipo (TArray (tref, _) = (
      add_str "array of "
      ppTipo !tref )
|   ppTipo (TRecord (fields, _) = (
      add_str "{ ";
      add_str " }" )

(*  Does all the basic pretty printing
  setup, the real work is in the call to
  `ppEnvEntry'`.
    ppEnvEntry: EnvEntry -> unit *)
fun ppEnvEntry ee = (
  begin_block ppstrm INCONSISTENT 0;
  ppEnvEntry' ee;
  end_block ppstrm;
  (* ppEnvEntry' ppstrm ee; *)
  flush_ppstream ppstrm;
  TextIO.output(TextIO.stdOut, "\n")
)

end

(*
datatype EnvEntry =
  VIntro (* int readonly *)
| Var of {ty: Tipo}
| Func of {
    level:   unit,
    label:   tigertemp.label (*string*),
    formals: Tipo list,
    result:  Tipo,
    extern:  bool (* Es para diferenciar
                     funciones de biblioteca. *)
  }
*)

(*
load "ejercicio7";
open ejercicio7;
open tigersres;
ppEnvEntry (Var {ty=TNil});
*)

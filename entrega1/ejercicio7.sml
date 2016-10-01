structure ejercicio7 :> ejercicio7 =
struct

open tigersres
open PP
open Utils
open tigertab

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
      ppTipo' tipo;
      add_str "}" )
|   ppEnvEntry' (Func {label=l, formals=fs, result=res, extern=e, ...}) = (
      add_str "function ";
      add_str l;
      add_str "(";
      ppFormals fs;
      add_str ") : ";
      ppTipo' res )
 (* ppTipo': Tipo -> unit *)
and ppTipo' TUnit =
      add_str "unit"
|   ppTipo' TNil =
      add_str "nil"
|   ppTipo' TInt =
      add_str "int"
|   ppTipo' TString =
      add_str "string"
|   ppTipo' (TArray (tref, _)) = (
      add_str "array of ";
      ppTipo' (!tref) )
|   ppTipo' (TRecord (fields, _)) =
      let
        fun ppField (name, typeref, _) = (
          add_str name;
          add_str ": ";
          ppTipo' (!typeref);
          add_str ", ")
      in (
        add_str "{ ";
        map ppField fields;
        add_str " }" )
      end
|   ppTipo' (TTipo (str)) =
      add_str str
and ppFormals []      = ()
|   ppFormals (x::[]) = ppTipo' x
|   ppFormals (x::xs) = (
      ppTipo' x;
      add_str ", ";
      ppFormals xs )

(*  Does all the basic pretty printing
  setup, the real work is in the call to
  `ppEnvEntry'`.
    ppEnvEntry: EnvEntry -> unit *)
fun ppEnvEntry ee = (
  begin_block ppstrm INCONSISTENT 0;
  ppEnvEntry' ee;
  end_block ppstrm;
  flush_ppstream ppstrm;
  TextIO.output(TextIO.stdOut, "\n")
)

fun ppTipo t = (
  begin_block ppstrm INCONSISTENT 0;
  ppTipo' t;
  end_block ppstrm;
  flush_ppstream ppstrm;
  TextIO.output(TextIO.stdOut, "\n")
)

fun tabPrint(f, g, tab) = (
  let
    fun f' x = (f x; print ":\t\t")
  in
    print "======= Tabla =======\n";
    tabAAplica(f', g, tab);
    ()
  end )

end

(*
load "ejercicio7";
open ejercicio7;
open tigersres;
ppEnvEntry (Var {ty=TNil});
*)

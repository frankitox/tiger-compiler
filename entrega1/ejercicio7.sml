structure ejercicio7 :> ejercicio7 =
struct

open tigersres;
open PP;

var current_indentation = ref 0;
var indentation_size    = 2;

fun break_right pps = (
  add_break pps (0, 1);
  begin_block pps CONSISTENT 2 )
fun break_left pps = (
  add_break pps (0, 1);
  end_block pps )

fun ppEnvEntry' pps ee =
  let
    fun
      ppee VIntro = add_string pps "VIntro"
    | ppee (Var {ty=tipo}) = (
        add_string pps "Var {";
        add_newline pps;
        add_break pps (10, 0);
        add_string pps "ty: HI afwoiafmwoaifaw oawi aoiw ao wiao iwoawo awo iawoi aow ia  woa owai aowiaoiw ao iwoia aw faowif aowfia wfoiawf aowif aowif aowfi awoifawofi awf iw";
        end_block pps;
        add_newline pps;
        add_string pps "}" )
    | ppee (Func record) = (
        add_string pps "Func {";
        begin_block pps CONSISTENT 2;
        add_string pps "HOLA";
        end_block pps;
        add_string pps "}" )
  in
    begin_block pps CONSISTENT 0;
    add_newline pps;
    ppee ee;
    end_block pps
  end

val ppstrm =
  PP.mk_ppstream {
    consumer  =
      fn s => TextIO.output(TextIO.stdOut, s), 
    linewidth = 79,
    flush =
      fn() => TextIO.flushOut TextIO.stdOut
  }

fun ppEnvEntry ee =
  ( ppEnvEntry' ppstrm ee;
    flush_ppstream ppstrm;
    TextIO.output(TextIO.stdOut, "\n") )

end

(*
load "ejercicio7";
open ejercicio7;
open tigersres;
ppEnvEntry (Var {ty=TNil});
*)

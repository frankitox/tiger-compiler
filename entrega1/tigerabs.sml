structure tigerabs = 
struct

type symbol = string
type pos = int
(* El escape indica si una funcion es usada por una funcion anidada. Ref significa que puede mutar *)
datatype var =
    SimpleVar    of symbol       (* x *)
  | FieldVar     of var * symbol (* x.y.z  == FieldVar(x.y, z) *)
  | SubscriptVar of var * exp    (* x.y[z] == SubscriptVar(x.y, z) *)

and exp =
    VarExp    of var * pos
  | UnitExp   of pos
  | NilExp    of pos
  | IntExp    of int * pos
  | StringExp of string * pos
  | CallExp   of {func: symbol, args: exp list} * pos
  | OpExp     of {left: exp, oper: oper, right: exp} * pos
  | RecordExp of {fields: (symbol * exp) list, typ: symbol} * pos   (* let  type R={i:int,next:R}
                                           var r:R:=R{i=10,next=nil} *)
  | SeqExp    of exp list * pos
  | AssignExp of {var: var, exp: exp} * pos
  | IfExp     of {test: exp, then': exp, else': exp option} * pos (* then' porque then es una palabra clave. option porque puede tener o no el else *)
  | WhileExp  of {test: exp, body: exp} * pos
  | ForExp    of {var: symbol, escape: bool ref, lo: exp, hi: exp, body: exp} * pos
  | LetExp    of {decs: dec list, body: exp} * pos
  | BreakExp  of pos
  | ArrayExp  of {typ: symbol, size: exp, init: exp} * pos  (* let type A = array of int
                                  var a:=A[100] of 1 *)

and dec =
    FunctionDec of ({name: symbol, params: field list, result: symbol option, body: exp} * pos) list
  | VarDec      of {name: symbol, escape: bool ref, typ: symbol option, init: exp} * pos
  | TypeDec     of ({name: symbol, ty: ty} * pos) list

and ty =
    NameTy   of symbol
  | RecordTy of field list
  | ArrayTy  of symbol

and oper = PlusOp | MinusOp | TimesOp | DivideOp | EqOp | NeqOp | LtOp | LeOp | GtOp | GeOp

withtype field = {name: symbol, escape: bool ref, typ: ty}
end

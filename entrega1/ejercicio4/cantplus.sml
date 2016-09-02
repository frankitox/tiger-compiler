structure cantplus :> cantplus =
struct

open tigerabs
open Int (* Lo abro al pedo, pa no romper
            el highlighting *)

(*
 * Helper functions: *)
val sum = foldl op+ 0
fun (* ifSomeElseZero: int option -> int *)
    ifSomeElseZero (SOME n) = n
|   ifSomeElseZero (NONE)   = 0

fun (* cantplus: exp -> int *)
  cantplus (VarExp(v, _)) = cantplusVar v
| cantplus (UnitExp(_))   = 0
| cantplus (NilExp(_))    = 0
| cantplus (IntExp(_))    = 0
| cantplus (StringExp(_)) = 0

| cantplus (CallExp ({args=args, ...}, _)) =
    sum (map cantplus args)

| cantplus (OpExp({left=l, oper=PlusOp, right=r}, _)) =
    1 + cantplus l + cantplus r
| cantplus (OpExp({left=l, right=r, ...}, _)) =
    cantplus l + cantplus r

| cantplus (RecordExp({fields=fs, ...}, _)) =
    sum (map (fn (_, exp) => cantplus exp) fs)

| cantplus (SeqExp(exps, _)) =
    sum (map cantplus exps)

| cantplus (AssignExp({var=v, exp=e}, _)) =
    cantplusVar v + cantplus e

| cantplus (IfExp({test=i, then'=t, else'=e}, _)) =
    let
      val e' = Option.map cantplus e
    in
      cantplus i + cantplus t + ifSomeElseZero e'
    end
| cantplus (WhileExp({test=t, body=b}, _)) =
    cantplus t + cantplus b

| cantplus (ForExp({lo=low, hi=high, body=body, ...}, _)) =
    sum (map cantplus [low, high, body])

| cantplus (LetExp({decs=ds, body=b}, _)) =
    sum (cantplus b :: map cantplusDec ds)

| cantplus (BreakExp _) = 0

| cantplus (ArrayExp({size=s, init=i, ...}, _))   =
    cantplus s + cantplus i

and (* cantplusVar: var -> int *)
  cantplusVar (SimpleVar _)         = 0
| cantplusVar (FieldVar     (v, _)) = cantplusVar v
| cantplusVar (SubscriptVar (v, e)) =
    cantplusVar v + cantplus e

and (* cantplusDec: dec -> int *)
  cantplusDec (FunctionDec (ls)) =
    let fun getBody (record, _) = #body(record)
    in sum (map (cantplus o getBody) ls)
    end
| cantplusDec (VarDec ({init=exp, ...}, _)) =
    cantplus exp
| cantplusDec (TypeDec _) = 0

end

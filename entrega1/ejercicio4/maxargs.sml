structure maxargs :> maxargs =
struct

open tigerabs
open Int

(*
 * Helper functions: *)
val maximum = foldl max 0
fun max x y = if x < y then y else x
fun (* ifSomeElseZero: int option -> int *)
    ifSomeElseZero (SOME n) = n
|   ifSomeElseZero (NONE)   = 0

fun (* maxargs: exp -> int *)
  maxargs (VarExp(v, _)) = maxargsVar v
| maxargs (UnitExp(_))   = 0
| maxargs (NilExp(_))    = 0
| maxargs (IntExp(_))    = 0
| maxargs (StringExp(_)) = 0

(* No podría ser que un arg haga un print
   zarpado? *)
| maxargs (CallExp ({func=s, args=args}, _)) =
    if s = "print" then length args else 0

| maxargs (OpExp({left=l, right=r, ...}, _)) =
    max (maxargs l) (maxargs r)

| maxargs (RecordExp({fields=fs, ...}, _)) =
    maximum (map (fn (_, exp) => maxargs exp) fs)

| maxargs (SeqExp(exps, _)) =
    maximum (map maxargs exps)

| maxargs (AssignExp({var=v, exp=e}, _)) =
    max (maxargsVar v) (maxargs e)

| maxargs (IfExp({test=i, then'=t, else'=e}, _)) =
    let
      val e' = Option.map maxargs e
    in
      max (max (maxargs i) (maxargs t))
          (ifSomeElseZero e')
    end
| maxargs (WhileExp({test=t, body=b}, _)) =
    max (maxargs t) (maxargs b)

| maxargs (ForExp({lo=low, hi=high, body=body, ...}, _)) =
    maximum (map maxargs [low, high, body])

| maxargs (LetExp({decs=ds, body=b}, _)) =
    maximum ((maxargs b)::(map maxargsDec ds))

| maxargs (BreakExp _) = 0

| maxargs (ArrayExp({size=s, init=i, ...}, _))   =
    max (maxargs s) (maxargs i)

and (* maxargsVar: var -> int *)
  maxargsVar (SimpleVar _)         = 0
| maxargsVar (FieldVar     (v, _)) = maxargsVar v
| maxargsVar (SubscriptVar (v, e)) =
    max (maxargsVar v) (maxargs e)

and (* maxargsDec: dec -> int *)
  maxargsDec (FunctionDec (ls)) =
    let fun getBody (record, _) = #body(record)
    in maximum (map (maxargs o getBody) ls)
    end
| maxargsDec (VarDec ({init=exp, ...}, _)) =
    maxargs exp
| maxargsDec (TypeDec _) = 0

end

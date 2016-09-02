structure ejercicio4 :> ejercicio4 =
struct

open tigerabs
open Int

val maximum = foldl max 0
fun max x y = if x < y then y else x

fun getBodys []              = []
|   getBodys ((stuff,p)::xs) = ((fn (n,p,r,b) => b) stuff ) :: (getBodys xs)

(*fun max x y = if x > y then x else y*)

fun (* maxargs: exp -> int *)
    maxargs (VarExp(v, _)) = maxargsVar v
|   maxargs (UnitExp(_))   = 0
|   maxargs (NilExp(_))    = 0
|   maxargs (IntExp(_))    = 0
|   maxargs (StringExp(_)) = 0

|   maxargs (CallExp ({func=s, args=args}, _)) =
      if s = "print" then length args else 0

|   maxargs (OpExp({left=l, right=r, ...}, _)) =
      max (maxargs l) (maxargs r)

|   maxargs (RecordExp({fields=fs, ...}, _)) =
      maximum (map (fn (_, exp) => maxargs exp) fs)

|   maxargs (SeqExp(exps, _)) =
      maximum (map maxargs exps)

|   maxargs (AssignExp({var=v, exp=e}, _))    =
      max (maxargsExp e) (maxargsVar v)

|   maxargs (IfExp({i,t,e}, p))      = max (max (maxargsExp i) (maxargsExp t)) (maxargsExp e)
|   maxargs (WhileExp({t,b}, p))     = max (maxargsExp t) (maxargsExp b)
|   maxargs (ForExp({v,e,l,h,b}, p)) = max (max (maxargsExp l) (maxargsExp h)) (max (maxargsVar v) (maxargsExp b))
|   maxargs (LetExp({ds,b}, p))      = max (maxargsExp b) (maxargsListDec ds)
|   maxargs (ArrayExp({t,s,i}, p))   = max (maxargsExp s) (maxargsExp i)
|   maxargs _                        = 0
and maxargsVar (SubscriptVar (v, e)) = max (maxargsExp e) (maxargsVar v)
|   maxargsVar (FieldVar     (v, s)) = maxargsVar v
|   maxargsVar _                     = 0

(*fun maxargsVar (SubscriptVar (v, e)) = max (maxargsExp e) (maxargsVar v)
|   maxargsVar (FieldVar     (v, s)) = maxargsVar v
|   maxargsVar _                     = 0

and maxargsExp (CallExp ({s,args}, p))  = if s = "print" then length(args) else 0
|   maxargsExp (VarExp(v, p))           = maxargsVar v
|   maxargsExp (OpExp({l, o, r}, p))    = max (maxargsExp l) (maxargsExp r)
|   maxargsExp (RecordExp({fs, t}, p))  = maxargsListExp (map (fn (x,y) => y) fs)
|   maxargsExp (SeqExp(es, p))          = maxargsListExp es
|   maxargsExp (AssignExp({v,e}, p))    = max (maxargsExp e) (maxargsVar v)
|   maxargsExp (IfExp({i,t,e}, p))      = max (max (maxargsExp i) (maxargsExp t)) (maxargsExp e)
|   maxargsExp (WhileExp({t,b}, p))     = max (maxargsExp t) (maxargsExp b)
|   maxargsExp (ForExp({v,e,l,h,b}, p)) = max (max (maxargsExp l) (maxargsExp h)) (max (maxargsVar v) (maxargsExp b))
|   maxargsExp (LetExp({ds,b}, p))      = max (maxargsExp b) (maxargsListDec ds)
|   maxargsExp (ArrayExp({t,s,i}, p))   = max (maxargsExp s) (maxargsExp i)
|   maxargsExp _                        = 0*)
  
fun maxargsListExp []     = 0
|  maxargsListExp (e::es)  = max (maxargsExp e) (maxargsListExp es) 
fun maxargsListDec []     = 0
|  maxargsListDec (d::ds)  = max (maxargsDec e) (maxargsListDec es) 

fun maxargsDec VarDec( {n,e,t,i}, p )  = maxargsExp i
|  maxargsDec FunctionDec( ls )    = maxargsListExp (map getBodys ls) 
|  maxargsDec _             = 0

end

open tigerabs
fun puto [] = []
|	puto (x::xs) = xs
fun getBodys []        = []
|  getBodys ((stuff,p)::xs) = ((fn (n,p,r,b) => b) stuff ) :: (getBodys xs)

fun max x y = if x>y then x else y

fun maxargsVar (SubscriptVar (v, e)) = max (maxargsExp e) (maxargsVar v)
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
|   maxargsExp _                        = 0
  
fun maxargsListExp []     = 0
|  maxargsListExp (e::es)  = max (maxargsExp e) (maxargsListExp es) 
fun maxargsListDec []     = 0
|  maxargsListDec (d::ds)  = max (maxargsDec e) (maxargsListDec es) 

fun maxargsDec VarDec( {n,e,t,i}, p )  = maxargsExp i
|  maxargsDec FunctionDec( ls )    = maxargsListExp (map getBodys ls) 
|  maxargsDec _             = 0

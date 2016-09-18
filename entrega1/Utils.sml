(*   If this file grows too much, I should
   create a signature file. *)

structure Utils = struct

(*  Join a list of strings with a delimiter.
    join: string list -> string -> string *)
fun join (x::[]) _    = x
|   join (x::xs) glue = x ^ glue ^ join xs glue
|   join []      _    = ""

end

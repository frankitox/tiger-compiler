structure tigertips =
struct

type unique = unit ref
datatype Tipo =
  TUnit
| TNil
| TInt
| TString
| TArray  of Tipo ref * unique
| TRecord of (string * Tipo ref * int) list
             * unique
    (* The int is used to provide a different
      order to the fields *)
| TTipo   of string

end

type var = string

type expr = 
  |Var of var
  |Func of var * expr
  |Application of expr * expr

and environment = (var * expr) list

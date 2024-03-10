open LccTypes 
open Utils

let rec lookup env var = match env with 
  [] -> None
  |(v,e)::t -> if v = var then Some(e) else lookup t var

let rec equal ast1 ast2 = match (ast1, ast2) with
                          |(Var(a), Var(b)) -> if a = b then true else false
                          |(Func(str1, ast1), Func(str2, ast2)) -> if (str1 = str2 && (equal ast1 ast2)) then true else false 
                          |(Application(ast1,ast2), Application(ast3,ast4)) -> if ((equal ast1 ast3) && (equal ast2 ast4)) then true else false
                          |_ -> false


(* 
  env is an empty token list          Use this as an environment
  e is the ast/exp tree
  exp -> exp
*)
let rec reduce env e = match e with
  |Var(x) -> let a = lookup env x in
                    (match a with
                    |None -> Var(x)
                    |Some(x) -> x
                    )
  |Func(str, ast) -> Func(str, reduce env ast)
  |Application(Func(str,astInner), ast2) -> let replace = reduce env ast2 in reduce ((str, replace)::env) astInner
  |Application(ast1, ast2) -> Application(reduce env ast1, reduce env ast2)
  |_ -> raise (Failure ("Failed reduce"))

let rec laze env e = match e with
|Var(x) -> let a = lookup env x in
                  (match a with
                  |None -> Var(x) (* if it doesn't exist in the environment then create it*)
                  |Some(x) -> x   (* when x already exists in the environment just return x which is a Var(x) *)
                  )
|Func(str, ast) -> Func(str, laze env ast)
|Application(one, two) -> match one with
                          |Var(str) -> Application(laze env one, laze env two)
                          |Func(str, astInner) -> laze ((str, two)::env) astInner (* don't do the same as reduce b/c that would have it do the whole thing*)
                          |Application(ast1, ast2) -> Application(laze env one, laze env two)
|_ -> raise (Failure ("Failed laze"))



let rec eager env e = match e with
|Var(x) -> let a = lookup env x in
                  (match a with
                  |None -> Var(x)
                  |Some(x) -> x
                  )
|Func(str, ast) -> Func(str, eager env ast)
|Application(ast1, ast2) -> let result = eager env ast2 in (* if they are equal that means ast2 is a Var(x) *)
                                        if (equal result ast2) then                                           
                                          (match ast1 with
                                          |Var(x) -> Application(ast1, ast2)
                                          |Func(str, ast) -> laze env e
                                          |Application(ast3, ast4) -> Application(eager env ast1, ast2)
                                        )
                                        else Application(ast1, result)                                     
(* |Application(ast1, ast2) -> match ast2 with
                            |Var(x) -> laze env e
                            |Func(str, ast) -> Application(ast1, laze env ast2)
                            |Application(inast1, inast2) -> Application(ast1, eager env ast2) *)
|_ -> raise (Failure ("Failed eager"))
                              



(* utility function to give new number *)
let cntr = ref (-1)

let fresh () =
  cntr := !cntr + 1 ;
  !cntr

let rec isalpha e1 e2 = equal e1 e2

open LccTypes 
open Utils
open TokenTypes

(* Grammar for the project
  e -> x
     |(Lx.e)
     |(e e)
*)

let rec parse_help toks = match toks with
       |[] -> raise (Failure ("parsing failed"))
       |Tok_Var(x)::t -> Var(x), t (* if you find a letter return is as Var(letter) along with the rest of the token list*)
       |Tok_LParen::Tok_Lambda::Tok_Var(x)::Tok_Dot::t -> let ast,rest = parse_help t in 
                                                              (match rest with 
                                                              (* |Tok_RParen::Tok_LParen::t -> parse_help rest *)
                                                              |Tok_RParen::t -> Func(x, ast), t
                                                              )
                                                              (* 
                                                                The point of this inner match statement is to get rid of the 
                                                                right parentheses left over after the parse_help above

                                                                x is a string after Lambda, b is a token list, ast is ast/expr 
                                                                 t is a token list. In this case b became t after the match
                                                              *)               
                                                                                                              
       |Tok_LParen::t ->  let lefttree, rest = parse_help t in
                          let righttree, rest2 = parse_help rest in
                          (
                            match rest2 with
                            | Tok_RParen::t -> Application(lefttree, righttree), t
                          )
                          (*
                            The lefttree is recursively called first. It goes deeper into the token list via match 2 and 3.
                            Eventually it will find a match 1 and return all the way up to give lefttree its value. At that point
                            righttree will be recursively called and deal with the entire right side of the tree.
                            
                            The inner match is to deal with the leftover right parentheses in the token list. We know there is a leftover because
                            match 3 starts by finding a left parentheses match and we need to get rid of its corresponding right parentheses.
                          *)

       |_ -> raise (Failure ("parsing failed"))

(* (Lx.x)((Ly.yy)a) *)

let rec givelist x = 
  let tree, rest = parse_help x in
  if rest = [] then rest
  else rest

let rec parse toks = 
  let tree, rest = parse_help toks in
  if rest = [] then tree
  else raise (Failure ("parsing failed")) 


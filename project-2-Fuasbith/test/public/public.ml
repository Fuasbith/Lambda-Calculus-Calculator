open OUnit2
open Lcc.Lexer
open Lcc.Parser
open Lcc.Eval
open Lcc.TokenTypes
open Lcc.LccTypes

let public_lexer1 _ =
  let result = ([Tok_LParen; Tok_Lambda; Tok_Var "x"; Tok_Dot; Tok_Var "x"; Tok_RParen]) in
  let student = "(Lx.x)" |> tokenize in
  assert_equal student result ~msg:"public_lexer" 

let public_lexer2 _ =
  let result = ([Tok_Dot; Tok_Dot; Tok_Dot]) in
  let student = "..." |> tokenize in
  assert_equal student result ~msg:"public_lexer"

let public_lexer3 _ =
  let result = ([Tok_Lambda; Tok_Lambda; Tok_Dot; Tok_Var "x"; Tok_Var "y"; Tok_Dot; Tok_Var "x"; Tok_Lambda; Tok_RParen]) in
  let student = "L L.x y.x L)" |> tokenize in
  assert_equal student result ~msg:"public_lexer"

let public_parser1 _ =
  let result = Func("x",Var("x")) in 
  let input  = "(L x.x)" |> tokenize in
  let student = input |> parse in
  assert_equal student result ~msg:"public_parser"

let public_parser2 _ =
  let result = Application(Func("x",Var("x")),Var("a")) in 
  let input  = "((Lx.x)a)" |> tokenize in
  let student = input |> parse in
  assert_equal student result ~msg:"public_parser"

let public_parser3 _ =
  let result = Application (Func ("x", Var "x"), Func ("y", Var "y")) in 
  let input  = "((Lx.x) (Ly.y))" |> tokenize in
  let student = input |> parse in
  assert_equal student result ~msg:"public_parser"

let public_eval1 _ =
  (* (Lx.x)((Ly.yy)a) *)
  (* Full *)
  let result = Application(Var("a"),Var("a")) in
  let input = Application(Func("x",Var("x")),Application(Func("y",Application(Var("y"),Var("y"))),Var("a"))) in
  let student = reduce [] input in
  assert_equal student result ~msg:"public_eval1 full";
  (* Lazy *)
  let result = Application(Func("y",Application(Var("y"),Var("y"))),(Var("a"))) in
  let input = Application(Func("x",Var("x")),Application(Func("y",Application(Var("y"),Var("y"))),Var("a"))) in
  let student = laze [] input in
  assert_equal student result ~msg:"public_eval1 laze";
  (* Eager *)
  let result = Application(Func("x",Var("x")),Application(Var("a"),Var("a"))) in
  let input = Application(Func("x",Var("x")),Application(Func("y",Application(Var("y"),Var("y"))),Var("a"))) in
  let student = eager [] input in
  assert_equal student result ~msg:"public_eval1 eager"

let public_eval2 _ =
  (* x *)
  (* Full *)
  let result = (Var("x")) in 
  let input = "x" |> tokenize |> parse in 
  let student = reduce [] input in 
  assert_equal student result ~msg:"pubic_eval2 full";
  (* Lazy *)
  let result = (Var("x")) in 
  let input = "x" |> tokenize |> parse in 
  let student = laze [] input in 
  assert_equal student result ~msg:"pubic_eval2 lazy";
  (* Eager *)
  let result = (Var("x")) in 
  let input = "x" |> tokenize |> parse in 
  let student = eager [] input in 
  assert_equal student result ~msg:"pubic_eval2 eager"

let public_eval3 _ =
  (* ((Lx.x) a) *)
  (* Full *)
  let result = (Var("a")) in
  let input = Application(Func("x",Var("x")),Var("a")) in 
  let student = reduce [] input in
  assert_equal student result ~msg:"pubic_eval3 full";
  (* Lazy *)
  let result = (Var("a")) in
  let input = Application(Func("x",Var("x")),Var("a")) in 
  let student = laze [] input in
  assert_equal student result ~msg:"public_eval3 laze";
  (* Eager *)
  let result = (Var("a")) in
  let input = Application(Func("x",Var("x")),Var("a")) in 
  let student = eager [] input in
  assert_equal student result ~msg:"public_eval3 eager"

let public_eval4 _ =
  let input1 = isalpha ("a" |> tokenize |> parse) ("a" |> tokenize |> parse) in
  let input2 = isalpha ("a" |> tokenize |> parse) ("b" |> tokenize |> parse) in
  assert input1;
  assert (not input2)
  
  
let suite = 
  "public" >::: [
    "public_lexer1" >:: public_lexer1;
    "public_lexer2" >:: public_lexer2;
    "public_lexer3" >:: public_lexer3;
    "public_parser1" >:: public_parser1;
    "public_parser2" >:: public_parser2;
    "public_parser3" >:: public_parser3;
    "public_eval1" >:: public_eval1;
    "public_eval2" >:: public_eval2;
    "public_eval3" >:: public_eval3;
    "public_eval4" >:: public_eval4;
  ]

let _ = run_test_tt_main suite

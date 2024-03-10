exception InvalidInputException of string

type token =
  | Tok_RParen
  | Tok_LParen
  | Tok_Lambda
  | Tok_Dot
  | Tok_Var of string

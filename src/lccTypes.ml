exception InvalidInputException of string

(* lexer stuff *)
type lambda_token =
  | Lambda_RParen
  | Lambda_LParen
  | Lambda_Lambda
  | Lambda_Dot
  | Lambda_Var of string
  | Lambda_EOF

type engl_token=
  | Engl_RParen
  | Engl_LParen
  | Engl_If
  | Engl_Then
  | Engl_Else
  | Engl_True
  | Engl_False
  | Engl_Not
  | Engl_And
  | Engl_Or
  | Engl_EOF

(* Parser stuff *)
type var = string
type lambda_ast = 
  |Var of var
  |Func of var * lambda_ast 
  |Application of lambda_ast* lambda_ast

type environment = (var * lambda_ast option) list

type engl_ast= 
  |If of engl_ast * engl_ast * engl_ast
  |Not of engl_ast
  |And of engl_ast * engl_ast
  |Or of engl_ast * engl_ast
  |Bool of bool

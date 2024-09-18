open LccTypes 

let string_of_token tok = match tok with 
| Engl_And -> "And"
| Engl_EOF -> "EOF"
| Engl_Else -> "Else"
| Engl_False -> "False"
| Engl_If -> "If"
| Engl_LParen -> "("
| Engl_Not -> "Not"
| Engl_Or -> "Or"
| Engl_RParen -> ")"
| Engl_Then -> "Then"
| Engl_True -> "True"
|_-> "didnt mat"

let rec string_of_list conv lst = 
  match lst with
  | [] -> ""
  | h::[] -> conv h
  | h::t -> (conv h) ^ " " ^ (string_of_list conv t)

let match_token (toks) (tok) =
  match toks with
  | [] -> raise (Failure("parsing failed"))
  | h::t when h = tok -> t
  | h::_ -> raise (Failure( 
      "parsing failed"))




let lookahead toks = match toks with
   h::t -> h
  | _ -> raise (Failure("Empty input to lookahead"))


(* Write your code below *)

let rec parse_lambda_help toks = 
  let (t,exp) = parse_E toks in 
  if t <> [Lambda_EOF] then raise (Failure("parsing failed")) else
    exp
  
and parse_E toks =
  match toks with 
  | Lambda_Var(a)::t -> (t, Var(a)) 
  | _ -> let toks = match_token toks Lambda_LParen in 
      match lookahead toks with 
      | Lambda_Lambda -> let toks = match_token toks Lambda_Lambda in
          let toks, c = match toks with 
            | Lambda_Var(b)::t-> (t, b)
            | _ -> raise (Failure("parsing failed")) in
          let toks = match_token toks Lambda_Dot in 
          let toks, d = parse_E toks in
          let toks = match_token toks Lambda_RParen in 
          (toks, Func(c, d))
      | _ -> let (t1, exp) = parse_E toks in 
          let (t2, exp2) = parse_E t1 in 
          let toks = match_token t2 Lambda_RParen in (toks, Application(exp, exp2)) 
                                                         
let parse_lambda toks = parse_lambda_help toks;;


let rec parse_engl_help toks =
  let (t,exp) = parse_C toks in 
  if t <> [Engl_EOF] then raise (Failure("parsing failed")) else 
    exp 
and parse_C toks =
  match lookahead toks with
  | Engl_If -> let toks = match_token toks Engl_If in 
               let (toks, e1) = parse_C toks in 
               let toks = match_token toks Engl_Then in 
               let (toks, e2) = parse_C toks in 
               let toks = match_token toks Engl_Else in 
               let (toks, e3) = parse_C toks in 
               (toks, If(e1, e2, e3))
  | _ -> parse_H toks 
and parse_H toks = match parse_U toks with 
  | (a,b) -> match lookahead a with 
    | Engl_And -> let toks = match_token a Engl_And in let (c,d) = parse_H toks in (c, And(b,d))
    | Engl_Or -> let toks = match_token a Engl_Or in let (c,d) = parse_H toks in (c, Or(b,d))
    | _ -> (a,b)
and parse_U toks = match lookahead toks with 
  | Engl_Not -> let toks = match_token toks Engl_Not in let (toks,exp) = parse_U toks in (toks, Not(exp))
  | _ -> parse_M toks

and parse_M toks = match lookahead toks with 
  | Engl_True -> let toks = match_token toks Engl_True in (toks, Bool(true))
  | Engl_False -> let toks = match_token toks Engl_False in (toks, Bool(false))
  | Engl_LParen -> let toks = match_token toks Engl_LParen in let (toks, exp) = parse_C toks in let toks = match_token toks Engl_RParen in (toks, exp)
  |_-> raise (Failure("parsing failed"))

  let parse_engl toks = parse_engl_help toks;;



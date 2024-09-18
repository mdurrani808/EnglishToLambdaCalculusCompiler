open LccTypes

let explode (s: string) : char list =
  let rec exp i l =
    if i < 0 then l else if s.[i] != ' ' then exp (i - 1) (s.[i] :: l) else exp (i-1) (l)
  in
  exp (String.length s - 1) [] ;;
let lex_lambda input = 
  let str_list = (explode input) in
  let rec lex_help input_list = match input_list with
    | [] -> [Lambda_EOF]
    | h::t -> match h with
      | '(' -> Lambda_LParen::lex_help t
      | ')' -> Lambda_RParen::lex_help t
      | 'L' -> Lambda_Lambda::lex_help t
      | '.' -> Lambda_Dot::lex_help t
      | _ -> let charac = (String.make 1 h) in if (Str.string_match (Str.regexp "[a-z]") charac 0) then Lambda_Var(charac)::lex_help t  else raise (Failure "tokenizing failed")
  in lex_help str_list


let lex_engl input =
  let length = String.length input in

  let rec tok pos =
    if pos >= length then
      [Engl_EOF]
    else if Str.string_match (Str.regexp "if") input pos then
      Engl_If::(tok (pos + 2))
    else if Str.string_match (Str.regexp "then") input pos then
      Engl_Then::(tok (pos + 4))
    else if Str.string_match (Str.regexp "else") input pos then
      Engl_Else::(tok (pos + 4))
    else if Str.string_match (Str.regexp "or") input pos then
      Engl_Or::(tok (pos + 2))
    else if Str.string_match (Str.regexp "and") input pos then
      Engl_And::(tok (pos + 3))
    else if Str.string_match (Str.regexp "true") input pos then
      Engl_True::(tok (pos + 4))
    else if Str.string_match (Str.regexp "false") input pos then
      Engl_False::(tok (pos + 5))
    else if Str.string_match (Str.regexp "not") input pos then
      Engl_Not::(tok (pos + 3))
    else if Str.string_match (Str.regexp "(") input pos then
      Engl_LParen::(tok (pos + 1))
    else if Str.string_match (Str.regexp ")") input pos then
      Engl_RParen::(tok (pos + 1))
    else if Str.string_match (Str.regexp "[ \n\t]") input pos then
      tok (pos + 1)
    else
      raise (Failure "tokenizing failed")

  in tok 0;;

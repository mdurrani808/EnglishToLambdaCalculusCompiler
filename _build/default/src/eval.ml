open LccTypes 

let cntr = ref (-1)

let fresh () =
  cntr := !cntr + 1 ;
  !cntr

let rec lookup env var = match env with 
  [] -> None
  |(v,e)::t -> if v = var then e else lookup t var;;
let rec substitute expr old_var new_var = match expr with
  | Var v -> if v = old_var then new_var else Var v
  | Func (v, body) ->
    if v = old_var then
      Func (v, body)  (* if v is the old_var, it's shadowed, so don't substitute in the body *)
    else
      Func (v, substitute body old_var new_var)  (* otherwise, substitute in the body *)
  | Application (func, arg) ->
    Application (substitute func old_var new_var, substitute arg old_var new_var)



let rec alpha_convert_aux e = match e with
  | Var(a) -> Var(a)
  | Func(a, b) -> 
    let new_var = string_of_int (fresh ()) in
    Func(new_var, alpha_convert_aux (substitute b a (Var(new_var))))
  | Application(a, b) -> Application(alpha_convert_aux a, alpha_convert_aux b)
  | _ -> e

let alpha_convert e =
  cntr := -1;
  alpha_convert_aux e
    

let rec isalpha e1 e2 = match (e1, e2) with
    | (Var v1, Var v2) -> v1 = v2
    | (Func (v1, body1), Func (v2, body2)) ->
      let fresh_var = string_of_int (fresh ()) in
      let new_body1 = substitute body1 v1 (Var fresh_var) in
      let new_body2 = substitute body2 v2 (Var fresh_var) in
      isalpha new_body1 new_body2
    | (Application (func1, arg1), Application (func2, arg2)) ->
      isalpha func1 func2 && isalpha arg1 arg2
    | _ -> false
  


let rec recursive_env env e = match env with 
  | [] -> e
  | (a,Some b)::t -> recursive_env t (substitute e a b)
(* Eager beta reduction function *)



let rec laze env e =
  match (alpha_convert e) with
  | Application (a, b) ->
      (match (a, b) with
      | (Func (fp1, fp2), b) -> substitute fp2 fp1 b
      | (c, d) -> Application (laze env c, laze env d))
  | _ -> e
(*let str = "(Lx.(((Ly.y)a)x))" in eager [] (parse_lambda((lex_lambda str)));;*)

let rec eager env e = match (alpha_convert e) with 
  | Var(a) -> Var(a)
  | Func(a, b) -> let b' = eager env b in Func(a, b')
  | Application(a,b) -> (match (a,b) with
    | (Func(a,b), c) -> let c' = eager env c in 
                        if not (isalpha c c') then Application(Func(a,b), c') 
                        else substitute b a c
    | (Var a, b) -> Application(Var a, eager env b)
    | (Application(a, b), c) -> Application(Application(a, b), eager env c)
    | _ -> e)
  | _ -> e;;


      (*| (Application(a,b), Func(c,d)) ->let c' = eager env (Func(c,d)) in
    if not (isalpha c' (Func(c,d))) then (
      (Application(Application(a,b), eager env (Func(c,d)))))
    else if (isalpha (Application(a,b)) (eager env (Application(a,b)))) then Application(Application(a,b), Func(c,d)) else (Application(eager env (Application(a,b)), Func(c,d)))*)
  (*let rec eager env e = match e with
  | Var(a) -> print_endline "1"; Var(a)
  | Func(a, Application(b,c)) -> print_endline "2";Func(a, eager env (Application(b,c)))
  (*| Application(Func(a,b), Func(c,d)) -> print_endline "3";Application(Func(a,b),eager env (Func(c,d)))*)
  | Application(Func(a,b), Var(c)) -> print_endline "4";substitute b a (Var(c))
  | Application(Func(a,b), c) -> 
    let c' = eager env c in
    if not (isalpha c' c) then (
      print_endline "5";(Application(Func(a,b), c')))
    else 
      (print_endline a;(substitute b a c))
  | Application(Application(a,b), Func(c,d)) -> let c' = eager env (Func(c,d)) in
    if not (isalpha c' (Func(c,d))) then (
      print_endline "5";(Application(Application(a,b), eager env (Func(c,d)))))
    else if (isalpha (Application(a,b)) (eager env (Application(a,b)))) then Application(Application(a,b), Func(c,d)) else (Application(eager env (Application(a,b)), Func(c,d)))
  | Application(a, b) ->  print_endline "7";Application(a, eager env (b))
  (*| Application(a, Func(b,c)) -> Application(a, eager env (Func(b,c)))*)
  | _ -> print_endline "8";e*)

(* Eager beta reduction function *)
(*let rec eager env e =
  let a = (alpha_convert e) in
  let rec eager_help env e_a =
  match (e_a) with
  | Application (a, b) ->
      (match (a, b) with
      | (Func (fp1, fp2), b) ->
          if (eager env b) = b then substitute fp2 fp1 b else Application (a, eager env b)
      | (c, d) -> Application (eager env c, eager env d))
  | Func (fp, body) -> Func (fp, eager env body) 
  | _ -> e_a in eager_help env a*)

  (*"((Lx.((x (Lx.(Ly.y))) (Lx.(Ly.x)))) (Lx.(Ly.x)))"*)
let rec reduce env e = 
  let ea = (alpha_convert e) in
  let rec reduce_help env e = 
    let e' = laze env e in
    if isalpha e' e then (recursive_env env e') else reduce_help env e' in reduce_help env (ea);;
  

let rec convert tree = match tree with
| Bool(true) -> "(Lx.(Ly.x))"
| Bool(false) -> "(Lx.(Ly.y))"
| If(a,b,c)-> "(("^(convert a)^" "^(convert b)^") "^(convert c)^")"
| Not(a)-> "((Lx.((x (Lx.(Ly.y))) (Lx.(Ly.x)))) "^(convert a)^")"
| And(a,b)-> "(((Lx.(Ly.((x y) (Lx.(Ly.y))))) "^(convert a)^") "^(convert b)^")"
| Or(a, b) ->"(((Lx.(Ly.((x (Lx.(Ly.x))) y))) "^(convert a)^") "^(convert b)^")"
let rec readable ast =
  match (alpha_convert ast) with
  |   Application(Func (a,Application (Application (Var b, Func (c, Func (d, Var e))),Func (f, Func (g, Var h)))),i)
  when a = b && d=e && f=h && c<>d && f<>g-> 
      "(not " ^ (readable (i)) ^ ")"
  |   Application(Application(Func (a,Func (b,Application (Application (Var c, Var d),Func (e, Func (f, Var g))))),h),i) -> "(" ^ (readable h) ^ " and " ^ (readable i) ^ ")"
  |   Application(Application(Func (a,Func (b,Application (Application (Var c, Func (d, Func (e, Var f))),Var g))),h),i) -> "(" ^ (readable h) ^ " or " ^ (readable i) ^ ")"
  | Func(a, Func(_, Var(x))) when a = x -> "true"
  | Func(_, Func(b, Var(y))) when b = y -> "false"
  | Application(Application(a, b), c) -> 
      "(if " ^ (readable (a)) ^ " then " ^ (readable (b)) ^ " else " ^ (readable (c)) ^ ")"
  | _ -> "wtf?"

(* let c1 = convert (parse_engl (lex_engl "(false and false) or (if true then true else false)")) in let a1 = parse_lambda(lex_lambda(convert(parse_engl(lex_engl(readable (parse_lambda((lex_lambda c1)))))))) in let a2= parse_lambda((lex_lambda c1)) in isalpha a1 a2;;*)

(*let str = "not true" in readable(reduce [] (parse_lambda 
                  (lex_lambda(convert (parse_engl (lex_engl str))))));;*)
(*let str = "not true" in eager [] (parse_lambda (lex_lambda(convert (parse_engl (lex_engl str)))));;*)

(*Application(Func ("2",Application(Application (Var "2",    Func ("5", Func ("6", Var "6"))),   Func ("3", Func ("4", Var "3")))),                 Func ("0", Func ("1", Var "0")));;*)


(*Application(Application(Func ("0", Func ("1", Var "0")),Func ("5", Func ("6", Var "6"))), Func ("3", Func ("4", Var "3")))*)
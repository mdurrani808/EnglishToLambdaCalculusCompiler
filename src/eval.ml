open LccTypes 

(* Counter for generating fresh variable names *)
let cntr = ref (-1)

(* Generate a fresh integer for variable names *)
let fresh () =
  cntr := !cntr + 1 ;
  !cntr

(* Look up a variable in the environment *)
let rec lookup env var = match env with 
  [] -> None
  |(v,e)::t -> if v = var then e else lookup t var

(* Substitute a new variable for an old one in an expression *)
let rec substitute expr old_var new_var = match expr with
  | Var v -> if v = old_var then new_var else Var v
  | Func (v, body) ->
    if v = old_var then
      Func (v, body)  (* Don't substitute in the body if the variable is shadowed *)
    else
      Func (v, substitute body old_var new_var) 
  | Application (func, arg) ->
    Application (substitute func old_var new_var, substitute arg old_var new_var)

(* Helper function for alpha conversion *)
let rec alpha_convert_help e = match e with
  | Var(a) -> Var(a)
  | Func(a, b) -> 
    let new_var = string_of_int (fresh ()) in
    Func(new_var, alpha_convert_help (substitute b a (Var(new_var))))
  | Application(a, b) -> Application(alpha_convert_help a, alpha_convert_help b)
  | _ -> e

(* Perform alpha conversion on an expression *)
let alpha_convert e =
  cntr := -1;
  alpha_convert_help e

(* Check if two expressions are alpha-equivalent *)
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

(* Apply the environment to an expression *)
let rec recursive_env env e = match env with 
  | [] -> e
  | (a,Some b)::t -> recursive_env t (substitute e a b)

(* Perform a single lazy evaluation step *)
let rec laze env e =
  match (alpha_convert e) with
  | Application (a, b) ->
      (match (a, b) with
      | (Func (fp1, fp2), b) -> substitute fp2 fp1 b
      | (c, d) -> Application (laze env c, laze env d))
  | _ -> e

(* Perform a single eager evaluation step *)
let rec eager env e = match e with 
  | Var(a) -> Var(a)
  | Func(a, b) -> let b' = eager env (b) in Func(a, b')
  | Application(a,b) -> (match (a,b) with
    | (Func(a,b), c) -> let c' = eager env c in if not (isalpha c' c) then ((Application(Func(a,b), c'))) else ((substitute b a c))
    | (a, b) ->  Application(eager env (a), eager env (b)))
  | _ -> e

(* Reduce an expression to its normal form *)
let rec reduce env e = 
  let ea = (alpha_convert e) in
  let rec reduce_help env e = 
    let e' = laze env e in
    if isalpha e' e then (recursive_env env e') else reduce_help env e' in reduce_help env (ea)

(* Convert English AST to Lambda Calculus string *)
let rec convert tree = match tree with
| Bool(true) -> "(Lx.(Ly.x))"
| Bool(false) -> "(Lx.(Ly.y))"
| If(a,b,c)-> "(("^(convert a)^" "^(convert b)^") "^(convert c)^")"
| Not(a)-> "((Lx.((x (Lx.(Ly.y))) (Lx.(Ly.x)))) "^(convert a)^")"
| And(a,b)-> "(((Lx.(Ly.((x y) (Lx.(Ly.y))))) "^(convert a)^") "^(convert b)^")"
| Or(a, b) ->"(((Lx.(Ly.((x (Lx.(Ly.x))) y))) "^(convert a)^") "^(convert b)^")"

(* Convert Lambda Calculus AST to readable English string *)
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
  | _ -> "Error!"
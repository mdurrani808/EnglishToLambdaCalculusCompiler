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

let rec alpha_conv expr count = match expr with
  | Var v -> Var v
  | Func (v, body) ->
    let new_v = string_of_int (count) in
    let new_body = alpha_conv (substitute body v (Var new_v)) (count+1) in
    Func (new_v, new_body)
  | Application (func, arg) ->
    let new_func = alpha_conv func (count+1) in
    let new_arg = alpha_conv arg (count+1) in
    Application (new_func, new_arg);;

let rec alpha_convert expr = 
  let cntr = cntr := -1 in
  let rec alpha_conv expr count = match expr with
  | Var v -> Var v
  | Func (v, body) ->
    let new_v = string_of_int (count) in
    let new_body = alpha_conv (substitute body v (Var new_v)) (fresh()) in
    Func (new_v, new_body)
  | Application (func, arg) ->
    let new_func = alpha_conv func (fresh()) in
    let new_arg = alpha_conv arg (fresh()) in
    Application (new_func, new_arg) in alpha_conv expr 0;;

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
  

let rec laze env e =
  match (e) with
  | Application (a, b) ->
      (match (a, b) with
      | (Func (fp1, fp2), b) -> substitute fp2 fp1 b
      | (c, d) -> Application (laze env c, laze env d))
  | _ -> e


let rec recursive_env env e = match env with 
  | [] -> e
  | (a,Some b)::t -> recursive_env t (substitute e a b)
(* Eager beta reduction function *)


let rec eager env e =
  let e_env = e in
  let rec eager_help env e =
  match ((*recursive_env env*) e) with
  | Application (a, b) ->
      (match (a, b) with
      | (Func (a,b), Func(c,d)) -> e
      | (Func (fp1, fp2), b) ->
          if (eager env b) = b then recursive_env env (substitute fp2 fp1 b) else Application (a, eager env b)
      | (c, d) -> Application (eager env c, eager env d))
  | Func (fp, body) -> Func (fp, eager env body) 
  | _ -> e in eager_help env e_env

let rec reduce env e = 
  let rec reduce_help env e = 
    let e' = eager env e in
    if e' = e then (recursive_env env e') else reduce_help env e' in reduce_help env (e);;
  

let rec convert tree = match tree with
| Bool(true) -> "(Lx.(Ly.x))"
| Bool(false) -> "(Lx.(Ly.y))"
| If(a,b,c)-> "(("^(convert a)^" "^(convert b)^") "^(convert c)^")"
| Not(a)-> "((Lx.((x (Lx.(Ly.y))) (Lx.(Ly.x)))) "^(convert a)^")"
| And(a,b)-> "(((Lx.(Ly.((x y) (Lx.(Ly.y))))) "^(convert a)^") "^(convert b)^")"
| Or(a, b) ->"(((Lx.(Ly.((x (Lx.(Ly.x))) y))) "^(convert a)^") "^(convert b)^")"
let rec readable ast =
  match ast with
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
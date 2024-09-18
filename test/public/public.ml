open OUnit2
open Lcc.Lexer
open Lcc.Parser
open Lcc.Eval
open Lcc.LccTypes

let rec general_test test_name f test_cases expecteds = 
  match (test_cases,expecteds) with
   [],[] -> true
  |tc::tcs,ex::exs -> let _ = assert_equal (f tc) ex ~msg:(test_name^": " ^ tc)
                      in  general_test test_name f tcs exs 

let rec eval_test test_name f test_cases expecteds = 
  match (test_cases,expecteds) with
   [],[] -> true
  |tc::tcs,ex::exs -> let _ = assert_equal (isalpha (f tc) ex) true ~msg:(test_name^": " ^ tc)
                      in  eval_test test_name f tcs exs 
(* -------------LEXER-------------- *)
let public_lex_lambda _ =
  let test_cases = ["(Lx.x)";
                    "...";
                    "L L.x y.x L)"] in
  let results =  [[Lambda_LParen; Lambda_Lambda; Lambda_Var "x"; Lambda_Dot; Lambda_Var "x"; Lambda_RParen;Lambda_EOF];
                  [Lambda_Dot; Lambda_Dot; Lambda_Dot; Lambda_EOF];
                  [Lambda_Lambda; Lambda_Lambda; Lambda_Dot; Lambda_Var "x"; Lambda_Var "y"; Lambda_Dot; Lambda_Var "x"; Lambda_Lambda; Lambda_RParen; Lambda_EOF]
                 ] in
  assert (general_test "public_lex_lambda" (lex_lambda) test_cases results)

let public_lex_engl _ = 
  let test_cases = ["true and or false";
                    "not  not     not)";
                    "(( if not ) and true false"] in

  let results =  [[Engl_True; Engl_And; Engl_Or; Engl_False; Engl_EOF];
                  [Engl_Not; Engl_Not; Engl_Not; Engl_RParen; Engl_EOF];
                  [Engl_LParen; Engl_LParen; Engl_If; Engl_Not; Engl_RParen; Engl_And; Engl_True; Engl_False; Engl_EOF]
                 ] in
  assert (general_test "public_lex_engl" (lex_engl) test_cases results)

(* -------------Parser-------------- *)
let public_parse_lambda _ =
  let test_cases = ["(Lx.x)";
                    "((Lx.x) a)";
                    "((Lx.x) (Ly.y))"] in
  let results =  [Func("x",Var("x"));
                  Application(Func("x",Var("x")),Var("a"));
                  Application (Func ("x", Var "x"), Func ("y", Var "y"))
                 ] in
  assert (general_test "public_parse_lambda" (fun x -> (parse_lambda (lex_lambda x))) test_cases results)


let public_parse_engl _ =
  let test_cases = ["not true";
                    "if true then false else true";
                    "false and (true or false)"] in

  let results =  [Not(Bool(true));
                  If(Bool(true), Bool(false), Bool(true));
                  And(Bool(false), Or(Bool(true), Bool(false)))
                 ] in
  assert (general_test "public_parse_engl" (fun x -> (parse_engl (lex_engl x))) test_cases results)


(* -------------INTERp-------------- *)

let test_cases = ["((Lx.x) ((Ly.(y y)) a))";
                  "x";
                  "((Lx.x) a)";
                  "((Lx.b) ((Ly.y) ((Lz.z) a)))";
                  ]

let public_reduce _ = 
  let results =  [Application(Var("a"),Var("a"));
                  Var("x");
                  Var("a");
                  Var("b");
                 ] in
  assert (eval_test "public_reduce" (fun x -> (reduce [] (parse_lambda (lex_lambda x)))) test_cases results)

let public_laze _ = 
  let results =  [Application(Func("y",Application(Var("y"),Var("y"))),Var("a"));
                  Var("x");
                  Var("a");
                  Var("b");
                 ] in
  assert (eval_test "public_laze" (fun x -> (laze [] (parse_lambda (lex_lambda x)))) test_cases results)

let public_eager _ = 
  let results =  [Application(Func("x",Var("x")),Application(Var("a"),Var("a")));
                  Var("x");
                  Var("a");
                  Application(Func("x",Var("b")),Application(Func("y",Var("y")),Var("a")));
                 ] in
  assert (eval_test "public_eager" (fun x -> (eager [] (parse_lambda (lex_lambda x)))) test_cases results)

let public_isalpha _ =
  let ina = "a" in let inb = "b" in
  let input1 = isalpha (ina |> lex_lambda |> parse_lambda) (ina |> lex_lambda |> parse_lambda) in
  let input2 = isalpha (ina |> lex_lambda |> parse_lambda) (inb |> lex_lambda |> parse_lambda) in
  assert_equal input1 true ~msg:("public_isalpha: " ^ ina ^ "," ^ ina);
  assert_equal input2 false ~msg:("public_isalpha: " ^ ina ^ "," ^ inb)


let public_convert _ = 
  let test_cases = ["false";
                    "not true";
                    "if true then not false else false"
                   ] in
  let results = ["(Lx.(Ly.y))";
                "((Lx.((x (Lx.(Ly.y))) (Lx.(Ly.x)))) (Lx.(Ly.x)))"; 
                "(((Lx.(Ly.x)) ((Lx.((x (Lx.(Ly.y))) (Lx.(Ly.x)))) (Lx.(Ly.y)))) (Lx.(Ly.y)))"
                ] in
  assert (general_test "public_convert" (fun x -> (convert (parse_engl (lex_engl x)))) test_cases results)
  
let public_readable _ = 
  let test_cases = ["(Lx.(Ly.y))";
                    "((Lx.((x (Lx.(Ly.y))) (Lx.(Ly.x)))) (Lx.(Ly.x)))";
                    "(((Lx.(Ly.x)) ((Lx.((x (Lx.(Ly.y))) (Lx.(Ly.x)))) (Lx.(Ly.y)))) (Lx.(Ly.y)))";
                    "(((Lx.(Ly.((x y) (Lx.(Ly.y))))) (Lx.(Ly.x))) (Lx.(Ly.y)))";
                    "(((Lx.(Ly.((x (Lx.(Ly.x))) y))) (Lx.(Ly.x))) (Lx.(Ly.y)))";
                   ] in
  let results = ["false";
                "(not true)"; 
                "(if true then (not false) else false)";
                "(true and false)";
                "(true or false)";
                ] in
  assert (general_test "public_readable" (fun x -> (readable (parse_lambda (lex_lambda x)))) test_cases results)


let cycle str = readable (reduce [] (parse_lambda 
                  (lex_lambda(convert (parse_engl (lex_engl str))))))

let public_test_all _ = 
  let test_cases = ["true";
                    "not true";
                   ] in
  let results = ["true";
                "false"
                ] in
  assert (general_test "public_all" (cycle) test_cases results)
(**)
let suite = 
  "public" >::: [
    "public_lex_lambda" >:: public_lex_lambda;
    "public_lex_engl" >:: public_lex_engl;
    "public_parse_lambda" >:: public_parse_lambda;
    "public_parse_engl" >:: public_parse_engl;
    "public_reduce" >:: public_reduce;
    "public_laze" >:: public_laze;
    "public_eager" >:: public_eager;
    "public_isalpha" >:: public_isalpha;
    "public_convert" >:: public_convert;
    "public_readable" >:: public_readable;
    "public_test_all" >:: public_test_all;
  ]

let _ = run_test_tt_main suite

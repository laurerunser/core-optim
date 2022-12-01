(* Build with `ocamlbuild -pkg alcotest simple.byte` *)
open Libfun

(* The tests *)
let test_ppshow () = 
  Alcotest.(check string) "same string" "(Terms.Var \"toto\")"
    (Libfun.Terms.(Format.asprintf "%a" pp_term (Var("toto"))))

let id = Syntax.fn "x" (TyFreeVar "X") (fun x -> x)
let poly_id = let open Syntax in
  ty_fn "X" (fun _X -> fn "x" _X (fun x -> x))
let test_fn_id () = Alcotest.(check string) "toto" "fun (x: X) = x" (Libfun.Terms.to_string id)
let test_fn_poly_id () = Alcotest.(check string) "toto" "fun [X] = (fun (x: X) = x)" (Libfun.Terms.to_string poly_id)

(* Pretty print tests for Types *)
let test_pretty_print expected ast = 
  Alcotest.(check string) expected expected ast
let test_print_ty_fun_simple () =
  test_pretty_print "x1 -> x2"
    (Types.to_string (TyFun (TyFreeVar "x1", TyFreeVar "x2")))
let test_print_ty_fun_double_left () =
  test_pretty_print "(x1 -> x2) -> x3"
    (Types.to_string (TyFun (TyFun (TyFreeVar "x1", TyFreeVar "x2"), TyFreeVar "x3")))
let test_print_ty_fun_double_right () =
  test_pretty_print "x1 -> (x2 -> x3)"
    (Types.to_string (TyFun (TyFreeVar "x1", TyFun (TyFreeVar "x2", TyFreeVar "x3"))))
let test_print_ty_fun_very_long () = (* to test line breaks and indents *)
      test_pretty_print "long_variable_name1 ->\n  (long_variable_name2 ->\n    (long_variable_name1 ->\n      (long_variable_name2 ->\n        (long_variable_name1 ->\n          (long_variable_name2 ->\n            (long_variable_name1 ->\n              (long_variable_name2 -> long_variable_name3)))))))"
        (Types.to_string (TyFun (TyFreeVar "long_variable_name1", TyFun (TyFreeVar "long_variable_name2", 
          TyFun (TyFreeVar "long_variable_name1", TyFun (TyFreeVar "long_variable_name2", 
          TyFun (TyFreeVar "long_variable_name1", TyFun (TyFreeVar "long_variable_name2", 
          TyFun (TyFreeVar "long_variable_name1", TyFun (TyFreeVar "long_variable_name2", TyFreeVar "long_variable_name3"))))))))))
    
let test_print_poly_type_simple() =
  test_pretty_print "forall x1. 0"
    (Types.to_string (PolymorphicType("x1", TyBoundVar 0)))
let test_print_poly_type_double() =
  test_pretty_print "forall x1. (forall x2. 1)"
    (Types.to_string (PolymorphicType("x1", PolymorphicType("x2", TyBoundVar 1))))

let test_print_poly_type_complex() =
  test_pretty_print "forall x1. (forall x2. (forall x3. (0 * 1 * 2)))"
    (Types.to_string (PolymorphicType("x1", PolymorphicType("x2", 
        PolymorphicType("x3", TyTuple([TyBoundVar 0; TyBoundVar 1; TyBoundVar 2]))))))

let test_print_ty_tuple_simple() =
  test_pretty_print "x * y * z"
    (Types.to_string (TyTuple [TyFreeVar "x"; TyFreeVar "y"; TyFreeVar "z"]))
let test_print_ty_tuple_double() =
  test_pretty_print "(x1 * x2 * x3) * y * z"
    (Types.to_string (TyTuple [
      TyTuple [TyFreeVar "x1"; TyFreeVar "x2"; TyFreeVar "x3"];
      TyFreeVar "y"; TyFreeVar "z"]))

let test_print_ty_compose1() =
  test_pretty_print "(forall x. (0 -> y)) -> (a * b)"
    (Types.to_string (TyFun(
      PolymorphicType("x", TyFun(TyBoundVar 0, TyFreeVar "y")),
      TyTuple [TyFreeVar "a"; TyFreeVar "b"])))
let test_print_ty_compose2() =
  test_pretty_print "(a * b) -> (forall x. (0 -> y))"
    (Types.to_string (TyFun(
      TyTuple [TyFreeVar "a"; TyFreeVar "b"],
      PolymorphicType("x", TyFun(TyBoundVar 0, TyFreeVar "y")))))
let test_print_ty_compose3() =
  test_pretty_print "forall x. ((a * b) -> (0 -> y))"
    (Types.to_string (PolymorphicType("x", TyFun(
      TyTuple [TyFreeVar "a"; TyFreeVar "b"],
      TyFun(TyBoundVar 0, TyFreeVar "y")))))
let test_print_ty_compose4() =
  test_pretty_print "forall x. ((a * (0 -> y)) -> b)"
    (Types.to_string (PolymorphicType("x", TyFun(
      TyTuple [TyFreeVar "a"; TyFun(TyBoundVar 0, TyFreeVar "y")],
      TyFreeVar "b"))))

(* Pretty print tests for Terms *)
let test_print_variable() = 
  test_pretty_print "toto"
  (Terms.to_string (Var("toto")))

let test_print_fun1() = 
  test_pretty_print "fun (x: ty) = y"
  (Terms.to_string (Fun("x", TyFreeVar("ty"), Var("y"))))

let test_print_fun2() = 
  test_pretty_print "fun (y: ty1) = z[ty2]"
  (Terms.to_string (Fun("y", TyFreeVar("ty1"), TypeApply(Var("z"), TyFreeVar("ty2")))))
let test_print_fun3() = 
  test_pretty_print "fun (x: forall poly_ty. (ty1 -> ty2)) = z"
  (Terms.to_string (Fun("x", 
    PolymorphicType("poly_ty", TyFun(TyFreeVar("ty1"), TyFreeVar("ty2"))),
    Var("z"))))
let test_print_fun4() = 
  test_pretty_print "fun (x: forall poly_ty. (ty1 -> (ty2 -> (ty3 -> ty4)))) =\n  (fun (y: ty4 -> ty5) = (fun (z: ty6 -> ty7) = z))"
  (Terms.to_string (Fun("x", 
    PolymorphicType("poly_ty", TyFun(TyFreeVar("ty1"), TyFun(TyFreeVar("ty2"), TyFun(TyFreeVar("ty3"), TyFreeVar("ty4"))))),
      Fun("y", TyFun(TyFreeVar("ty4"), TyFreeVar("ty5")), 
        Fun("z", TyFun(TyFreeVar("ty6"), TyFreeVar("ty7")), Var("z"))))))
let test_print_fun_apply1() =
  test_pretty_print "f x"
  (Terms.to_string (FunApply(Var("f"), Var("x"))))

let test_print_fun_apply2() =
  test_pretty_print "(x y) (fun (z: ty) = (z x))"
  (Terms.to_string (FunApply(FunApply(Var("x"), Var("y")), Fun("z", TyFreeVar("ty"), FunApply(Var("z"), Var("x"))))))

let test_print_let1() = 
  test_pretty_print "let x = y in z"
    (Terms.to_string (Let("x", Var("y"), Var("z"))))

let test_print_let2() = 
  test_pretty_print "let x = (f y) in g toto"
  (Terms.to_string (Let("x", FunApply(Var("f"), Var("y")), FunApply(Var("g"), Var("toto")))))

let test_print_let3() = 
  test_pretty_print "let x = (f y) in\ng let x = (f2 y2) in let x2 = (f3 y3) in g5 toto"
  (Terms.to_string (Let("x", FunApply(Var("f"), Var("y")), 
      FunApply(Var("g"), Let("x", 
      FunApply(Var("f2"), Var("y2")), 
      Let("x2", FunApply(Var("f3"), Var("y3")), 
      FunApply(Var("g5"), Var("toto"))))))))         

let test_print_type_abstraction1() = 
  test_pretty_print "fun [tyvar] = y"
  (Terms.to_string (TypeAbstraction("tyvar", Var("y"))))

let test_print_type_abstraction2() = 
  test_pretty_print "fun [tyvar] = (fun (x: ty2) = z)"
  (Terms.to_string (TypeAbstraction("tyvar", Fun("x", TyFreeVar("ty2"), Var("z")))))

let test_print_type_apply1() = 
  test_pretty_print "x[y]"
  (Terms.to_string (TypeApply(Var("x"), TyFreeVar("y"))))

let test_print_type_apply2() = 
  test_pretty_print "(fun (x: ty) = (g let x = (f2 y2) in y))[forall x.\n  ((a * (tyvar3 -> tyvar4)) -> b)]"
  (Terms.to_string (TypeApply(
      Fun("x", TyFreeVar("ty"),
        FunApply(Var("g"), Let("x", 
        FunApply(Var("f2"), Var("y2")), 
        Var("y")))), 
      PolymorphicType("x", TyFun(
        TyTuple [TyFreeVar "a"; TyFun(TyFreeVar "tyvar3", TyFreeVar "tyvar4")],
        TyFreeVar "b")))))

(* free_vars tests *)
let checkVarSet = Alcotest.(slist string String.compare)
let set_to_list = fun s -> List.of_seq (Terms.VarSet.to_seq s)
let list_to_string l = (* prints a list of string as [x1; x2; ...; xn;]*)
  "[" ^ List.fold_left (fun x acc -> acc ^ "; " ^ x) "" l ^ "]"

let test_free_vars l ast = 
  Alcotest.(check checkVarSet) (list_to_string l) l ast
let test_free_vars_var() =
  test_free_vars ["a"]
  (set_to_list (Terms.free_vars (Var "a")))

let test_free_vars_fun1() =
  test_free_vars ["a"]
  (set_to_list (Terms.free_vars (Fun("b", TyFreeVar "ty", Var "a"))))
let test_free_vars_fun2() =
  test_free_vars []
  (set_to_list (Terms.free_vars (Fun("b", TyFreeVar "ty", Fun("a", TyFreeVar "ty", Var "b")))))

let test_free_vars_funApply1() =
  test_free_vars ["a"; "b"]
  (set_to_list (Terms.free_vars (
    FunApply(Fun("a", TyFreeVar "ty", Var "b"), Fun("b", TyFreeVar "ty", Var "a")))))

let test_free_vars_funApply2() =
  test_free_vars ["z"; "x"; "y"; "v"]
  (set_to_list (Terms.free_vars (
    FunApply(
      FunApply(
        FunApply(
          FunApply(Var "y", Var "v"),
          Fun("x", TyFreeVar "ty", Var "x")),
        FunApply(
          FunApply(Var "z", Var "x"),
          FunApply(Var "y", Var "x"))),
      FunApply(
        Fun("z", TyFreeVar "ty", Var "x"),
        Fun("u", TyFreeVar "ty", Var "u"))))))


let test_free_vars_Let1() =
  test_free_vars ["a";"d";"e"]
  (set_to_list (Terms.free_vars (
    Let("a",
      FunApply(Fun ("b", TyFreeVar "ty", Var "a"),
      Let("c", Var "d", Var "c")), Var "e" ))))

let test_free_vars_Let2() = 
  test_free_vars ["a"; "b"]
  (set_to_list (Terms.free_vars (
    Let("x",
      Var "a",
      Let("y",
        FunApply(
          Fun ("z", TyFreeVar "ty", FunApply(Var "b", Var "x")),
          Var "x"),
        FunApply(Var "y", Var "x")))
  )))

let test_free_var_type_abstraction() =
  test_free_vars ["d";"e"]
  (set_to_list (Terms.free_vars (
    TypeAbstraction("ty", Let("a",
      FunApply(Fun ("b", TyFreeVar "ty", Var "e"),
      Let("c", Var "d", Var "c")), Var "a" )))))

  let test_free_var_type_apply() =
  test_free_vars ["a";"d";"e"]
  (set_to_list (Terms.free_vars (
    TypeApply(Let("a",
      FunApply(Fun ("b", TyFreeVar "ty", Var "a"),
      Let("c", Var "d", Var "c")), Var "e" ),
      TyFreeVar "ty"))))

let test_free_var_type_annotation() =
  test_free_vars ["a";"d";"e"]
  (set_to_list (Terms.free_vars (
    TypeAnnotation(Let("a",
      FunApply(Fun ("b", TyFreeVar "ty", Var "a"),
      Let("c", Var "d", Var "c")), Var "e" ),
      TyFreeVar "ty"))))

(* Run it *)
let () =
  let open Alcotest in
  run "Utils" [
      "test Var", [ test_case "String mashing" `Quick test_ppshow  ];
      "test fn", [
        test_case "fn id" `Quick test_fn_id;
        test_case "fn poly id" `Quick test_fn_poly_id
      ];
      "test print function type", [
        test_case "Simple" `Quick test_print_ty_fun_simple;
        test_case "Left assoc" `Quick test_print_ty_fun_double_left;
        test_case "Right assoc" `Quick test_print_ty_fun_double_right;
        test_case "With line breaks" `Quick test_print_ty_fun_very_long;
      ];
      "test print polymorphic type", [
        test_case "Simple" `Quick test_print_poly_type_simple;
        test_case "Double" `Quick test_print_poly_type_double;
        test_case "Complex" `Quick test_print_poly_type_complex;
      ];
      "test print tuple type", [
        test_case "Simple" `Quick test_print_ty_tuple_simple;
        test_case "Double" `Quick test_print_ty_tuple_double;
      ];
      "test print type compose", [
        test_case "Compose1" `Quick test_print_ty_compose1;
        test_case "Compose2" `Quick test_print_ty_compose2;
        test_case "Compose3" `Quick test_print_ty_compose3;
        test_case "Compose4" `Quick test_print_ty_compose4;
      ];
      "test print variable", [
        test_case "Variable" `Quick test_print_variable;
      ];
      "test print fun", [
        test_case "Fun1" `Quick test_print_fun1;
        test_case "Fun2" `Quick test_print_fun2;
        test_case "Fun3" `Quick test_print_fun3;
        test_case "Fun4" `Quick test_print_fun4;
      ];
      "test print fun apply", [
        test_case "FunApply1" `Quick test_print_fun_apply1;
        test_case "FunApply2" `Quick test_print_fun_apply2;
      ];
      "test print let", [
        test_case "Let1" `Quick test_print_let1;
        test_case "Let2" `Quick test_print_let2;
        test_case "Let3" `Quick test_print_let3;
      ];
      "test print type abstraction", [
        test_case "TypeAbstraction1" `Quick test_print_type_abstraction1;
        test_case "TypeAbstraction2" `Quick test_print_type_abstraction2;
      ];
      "test print type apply", [
        test_case "TypeApply1" `Quick test_print_type_apply1;
        test_case "TypeApply2" `Quick test_print_type_apply2;
      ];
      "test free_vars", [
        test_case "Var" `Quick test_free_vars_var;
        test_case "Fun1" `Quick test_free_vars_fun1;
        test_case "Fun2" `Quick test_free_vars_fun2;
        test_case "FunApply1" `Quick test_free_vars_funApply1;
        test_case "FunApply2" `Quick test_free_vars_funApply2;
        test_case "Let1" `Quick test_free_vars_Let1;
        test_case "Let2" `Quick test_free_vars_Let2;
        test_case "Type abstraction" `Quick test_free_var_type_abstraction;
        test_case "TypleApply" `Quick test_free_var_type_apply;
        test_case "TypeAnnotation" `Quick test_free_var_type_annotation
      ]
    ]
    
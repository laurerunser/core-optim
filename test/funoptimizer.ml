(* Build with `ocamlbuild -pkg alcotest simple.byte` *)
open Libfun

(* The tests *)
let test_ppshow () = 
  Alcotest.(check string) "same string" "(Terms.Var \"toto\")"
    (Libfun.Terms.(Format.asprintf "%a" pp_term (Var("toto"))))

(* Pretty print tests for Types *)
let test_print_ty_fun_simple () =
  Alcotest.(check string) "same string" "x1 -> x2"
    (Types.to_string (TyFun (TyVar "x1", TyVar "x2")))
let test_print_ty_fun_double_left () =
  Alcotest.(check string) "same string" "(x1 -> x2) -> x3"
    (Types.to_string (TyFun (TyFun (TyVar "x1", TyVar "x2"), TyVar "x3")))
let test_print_ty_fun_double_right () =
  Alcotest.(check string) "same string" "x1 -> (x2 -> x3)"
    (Types.to_string (TyFun (TyVar "x1", TyFun (TyVar "x2", TyVar "x3"))))
let test_print_ty_fun_very_long () = (* to test line breaks and indents *)
      Alcotest.(check string) "same string" "long_variable_name1 ->\n  (long_variable_name2 ->\n    (long_variable_name1 ->\n      (long_variable_name2 ->\n        (long_variable_name1 ->\n          (long_variable_name2 ->\n            (long_variable_name1 ->\n              (long_variable_name2 -> long_variable_name3)))))))"
        (Types.to_string (TyFun (TyVar "long_variable_name1", TyFun (TyVar "long_variable_name2", 
          TyFun (TyVar "long_variable_name1", TyFun (TyVar "long_variable_name2", 
          TyFun (TyVar "long_variable_name1", TyFun (TyVar "long_variable_name2", 
          TyFun (TyVar "long_variable_name1", TyFun (TyVar "long_variable_name2", TyVar "long_variable_name3"))))))))))
    
let test_print_poly_type_simple() =
  Alcotest.(check string) "same string" "forall x1. x1"
    (Types.to_string (PolymorphicType("x1", TyVar "x1")))
let test_print_poly_type_double() =
  Alcotest.(check string) "same string" "forall x1. (forall x2. x1)"
    (Types.to_string (PolymorphicType("x1", PolymorphicType("x2", TyVar "x1"))))

let test_print_ty_tuple_simple() =
  Alcotest.(check string) "same string" "x * y * z"
    (Types.to_string (TyTuple [TyVar "x"; TyVar "y"; TyVar "z"]))
let test_print_ty_tuple_double() =
  Alcotest.(check string) "same string" "(x1 * x2 * x3) * y * z"
    (Types.to_string (TyTuple [
      TyTuple [TyVar "x1"; TyVar "x2"; TyVar "x3"];
      TyVar "y"; TyVar "z"]))

let test_print_ty_compose1() =
  Alcotest.(check string) "same string" "(forall x. (x -> y)) -> (a * b)"
    (Types.to_string (TyFun(
      PolymorphicType("x", TyFun(TyVar "x", TyVar "y")),
      TyTuple [TyVar "a"; TyVar "b"])))
let test_print_ty_compose2() =
  Alcotest.(check string) "same string" "(a * b) -> (forall x. (x -> y))"
    (Types.to_string (TyFun(
      TyTuple [TyVar "a"; TyVar "b"],
      PolymorphicType("x", TyFun(TyVar "x", TyVar "y")))))
let test_print_ty_compose3() =
  Alcotest.(check string) "same string" "forall x. ((a * b) -> (x -> y))"
    (Types.to_string (PolymorphicType("x", TyFun(
      TyTuple [TyVar "a"; TyVar "b"],
      TyFun(TyVar "x", TyVar "y")))))
let test_print_ty_compose4() =
  Alcotest.(check string) "same string" "forall x. ((a * (x -> y)) -> b)"
    (Types.to_string (PolymorphicType("x", TyFun(
      TyTuple [TyVar "a"; TyFun(TyVar "x", TyVar "y")],
      TyVar "b"))))

(* Pretty print tests for Terms *)
let test_print_variable() = 
  Alcotest.(check string) "same string" "toto"
  (Terms.to_string (Var("toto")))

let test_print_fun1() = 
  Alcotest.(check string) "same string" "fun (x: ty) = y"
  (Terms.to_string (Fun("x", TyVar("ty"), Var("y"))))

let test_print_fun2() = 
  Alcotest.(check string) "same string" "fun (y: ty1) = z[ty2]"
  (Terms.to_string (Fun("y", TyVar("ty1"), TypeApply(Var("z"), TyVar("ty2")))))
let test_print_fun3() = 
  Alcotest.(check string) "same string" "fun (x: forall poly_ty. (ty1 -> ty2)) = z"
  (Terms.to_string (Fun("x", 
    PolymorphicType("poly_ty", TyFun(TyVar("ty1"), TyVar("ty2"))),
    Var("z"))))
let test_print_fun4() = 
  Alcotest.(check string) "same string" "fun (x: forall poly_ty. (ty1 -> (ty2 -> (ty3 -> ty4)))) =\n  (fun (y: ty4 -> ty5) = (fun (z: ty6 -> ty7) = z))"
  (Terms.to_string (Fun("x", 
    PolymorphicType("poly_ty", TyFun(TyVar("ty1"), TyFun(TyVar("ty2"), TyFun(TyVar("ty3"), TyVar("ty4"))))),
      Fun("y", TyFun(TyVar("ty4"), TyVar("ty5")), 
        Fun("z", TyFun(TyVar("ty6"), TyVar("ty7")), Var("z"))))))
let test_print_fun_apply1() =
  Alcotest.(check string) "same string" "f x"
  (Terms.to_string (FunApply(Var("f"), Var("x"))))

let test_print_fun_apply2() =
  Alcotest.(check string) "same string" "(x y) (fun (z: ty) = (z x))"
  (Terms.to_string (FunApply(FunApply(Var("x"), Var("y")), Fun("z", TyVar("ty"), FunApply(Var("z"), Var("x"))))))

let test_print_let1() = 
  Alcotest.(check string) "same string" "let x = y in z"
    (Terms.to_string (Let("x", Var("y"), Var("z"))))

let test_print_let2() = 
  Alcotest.(check string) "same string" "let x = (f y) in g toto"
  (Terms.to_string (Let("x", FunApply(Var("f"), Var("y")), FunApply(Var("g"), Var("toto")))))

let test_print_let3() = 
  Alcotest.(check string) "same string" "let x = (f y) in\ng let x = (f2 y2) in let x2 = (f3 y3) in g5 toto"
  (Terms.to_string (Let("x", FunApply(Var("f"), Var("y")), 
      FunApply(Var("g"), Let("x", 
      FunApply(Var("f2"), Var("y2")), 
      Let("x2", FunApply(Var("f3"), Var("y3")), 
      FunApply(Var("g5"), Var("toto"))))))))         

let test_print_type_abstraction1() = 
  Alcotest.(check string) "same string" "fun [tyvar] = y"
  (Terms.to_string (TypeAbstraction("tyvar", Var("y"))))

let test_print_type_abstraction2() = 
  Alcotest.(check string) "same string" "fun [tyvar] = (fun (x: ty2) = z)"
  (Terms.to_string (TypeAbstraction("tyvar", Fun("x", TyVar("ty2"), Var("z")))))

let test_print_type_apply1() = 
  Alcotest.(check string) "same string" "x[y]"
  (Terms.to_string (TypeApply(Var("x"), TyVar("y"))))

let test_print_type_apply2() = 
  Alcotest.(check string) "same string" "(fun (x: ty) = (g let x = (f2 y2) in y))[forall x.\n  ((a * (tyvar3 -> tyvar4)) -> b)]"
  (Terms.to_string (TypeApply(
      Fun("x", TyVar("ty"),
        FunApply(Var("g"), Let("x", 
        FunApply(Var("f2"), Var("y2")), 
        Var("y")))), 
      PolymorphicType("x", TyFun(
        TyTuple [TyVar "a"; TyFun(TyVar "tyvar3", TyVar "tyvar4")],
        TyVar "b")))))

(* free_vars tests *)
let checkVarSet = Alcotest.(slist string String.compare)
let set_to_list = fun s -> List.of_seq (Terms.VarSet.to_seq s)
let test_free_vars_var() =
  Alcotest.(check checkVarSet) "same set" ["a"]
  (set_to_list (Terms.free_vars (Var "a")))

let test_free_vars_fun1() =
  Alcotest.(check checkVarSet) "same set" ["a"]
  (set_to_list (Terms.free_vars (Fun("b", TyVar "ty", Var "a"))))
let test_free_vars_fun2() =
  Alcotest.(check checkVarSet) "same set" []
  (set_to_list (Terms.free_vars (Fun("b", TyVar "ty", Fun("a", TyVar "ty", Var "b")))))

let test_free_vars_funApply1() =
  Alcotest.(check checkVarSet) "same set" ["a"; "b"]
  (set_to_list (Terms.free_vars (
    FunApply(Fun("a", TyVar "ty", Var "b"), Fun("b", TyVar "ty", Var "a")))))

let test_free_vars_funApply2() =
  Alcotest.(check checkVarSet) "same set" ["z"; "x"; "y"; "v"]
  (set_to_list (Terms.free_vars (
    FunApply(
      FunApply(
        FunApply(
          FunApply(Var "y", Var "v"),
          Fun("x", TyVar "ty", Var "x")),
        FunApply(
          FunApply(Var "z", Var "x"),
          FunApply(Var "y", Var "x"))),
      FunApply(
        Fun("z", TyVar "ty", Var "x"),
        Fun("u", TyVar "ty", Var "u"))))))


let test_free_vars_Let1() =
  Alcotest.(check checkVarSet) "same set" ["a";"d";"e"]
  (set_to_list (Terms.free_vars (
    Let("a",
      FunApply(Fun ("b", TyVar "ty", Var "a"),
      Let("c", Var "d", Var "c")), Var "e" ))))

let test_free_vars_Let2() = 
  Alcotest.(check checkVarSet) "same set" ["a"; "b"]
  (set_to_list (Terms.free_vars (
    Let("x",
      Var "a",
      Let("y",
        FunApply(
          Fun ("z", TyVar "ty", FunApply(Var "b", Var "x")),
          Var "x"),
        FunApply(Var "y", Var "x")))
  )))

let test_free_var_type_abstraction() =
  Alcotest.(check checkVarSet) "same set" ["a";"d";"e"]
  (set_to_list (Terms.free_vars (
    TypeAbstraction("ty", Let("a",
      FunApply(Fun ("b", TyVar "ty", Var "a"),
      Let("c", Var "d", Var "c")), Var "e" )))))

  let test_free_var_type_apply() =
  Alcotest.(check checkVarSet) "same set" ["a";"d";"e"]
  (set_to_list (Terms.free_vars (
    TypeApply(Let("a",
      FunApply(Fun ("b", TyVar "ty", Var "a"),
      Let("c", Var "d", Var "c")), Var "e" ),
      TyVar "ty"))))

let test_free_var_type_annotation() =
  Alcotest.(check checkVarSet) "same set" ["a";"d";"e"]
  (set_to_list (Terms.free_vars (
    TypeAnnotation(Let("a",
      FunApply(Fun ("b", TyVar "ty", Var "a"),
      Let("c", Var "d", Var "c")), Var "e" ),
      TyVar "ty"))))

(* template
let test_print_() = 
  Alcotest.(check string) "same string" ""
  (Terms.to_string )
  *)


(* Run it *)
let () =
  let open Alcotest in
  run "Utils" [
      "test Var", [ test_case "String mashing" `Quick test_ppshow  ];
      "test print function type", [
        test_case "Simple" `Quick test_print_ty_fun_simple;
        test_case "Left assoc" `Quick test_print_ty_fun_double_left;
        test_case "Right assoc" `Quick test_print_ty_fun_double_right;
        test_case "With line breaks" `Quick test_print_ty_fun_very_long;
      ];
      "test print polymorphic type", [
        test_case "Simple" `Quick test_print_poly_type_simple;
        test_case "Double" `Quick test_print_poly_type_double;
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
    
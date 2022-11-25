(* Build with `ocamlbuild -pkg alcotest simple.byte` *)
open Libfun.Types

(* The tests *)
let test_ppshow () = 
  Alcotest.(check string) "same string" "(Terms.Var \"toto\")"
    (Libfun.Terms.(Format.asprintf "%a" pp_term (Var("toto"))))

let test_print_ty_fun_simple () =
  Alcotest.(check string) "same string" "x1 -> x2"
    (to_string (TyFun (TyVar "x1", TyVar "x2")))
let test_print_ty_fun_double_left () =
  Alcotest.(check string) "same string" "(x1 -> x2) -> x3"
    (to_string (TyFun (TyFun (TyVar "x1", TyVar "x2"), TyVar "x3")))
let test_print_ty_fun_double_right () =
  Alcotest.(check string) "same string" "x1 -> (x2 -> x3)"
    (to_string (TyFun (TyVar "x1", TyFun (TyVar "x2", TyVar "x3"))))

let test_print_poly_type_simple() =
  Alcotest.(check string) "same string" "forall x1. x1"
    (to_string (PolymorphicType("x1", TyVar "x1")))
let test_print_poly_type_double() =
  Alcotest.(check string) "same string" "forall x1. (forall x2. x1)"
    (to_string (PolymorphicType("x1", PolymorphicType("x2", TyVar "x1"))))

let test_print_ty_tuple_simple() =
  Alcotest.(check string) "same string" "x * y * z"
    (to_string (TyTuple [TyVar "x"; TyVar "y"; TyVar "z"]))
let test_print_ty_tuple_double() =
  Alcotest.(check string) "same string" "(x1 * x2 * x3) * y * z"
    (to_string (TyTuple [
      TyTuple [TyVar "x1"; TyVar "x2"; TyVar "x3"];
      TyVar "y"; TyVar "z"]))

let test_print_ty_compose1() =
  Alcotest.(check string) "same string" "(forall x. (x -> y)) -> (a * b)"
    (to_string (TyFun(
      PolymorphicType("x", TyFun(TyVar "x", TyVar "y")),
      TyTuple [TyVar "a"; TyVar "b"])))
let test_print_ty_compose2() =
  Alcotest.(check string) "same string" "(a * b) -> (forall x. (x -> y))"
    (to_string (TyFun(
      TyTuple [TyVar "a"; TyVar "b"],
      PolymorphicType("x", TyFun(TyVar "x", TyVar "y")))))
let test_print_ty_compose3() =
  Alcotest.(check string) "same string" "forall x. ((a * b) -> (x -> y))"
    (to_string (PolymorphicType("x", TyFun(
      TyTuple [TyVar "a"; TyVar "b"],
      TyFun(TyVar "x", TyVar "y")))))
let test_print_ty_compose4() =
  Alcotest.(check string) "same string" "forall x. ((a * (x -> y)) -> b)"
    (to_string (PolymorphicType("x", TyFun(
      TyTuple [TyVar "a"; TyFun(TyVar "x", TyVar "y")],
      TyVar "b"))))

  (* Run it *)
let () =
  let open Alcotest in
  run "Utils" [
      "test Var", [ test_case "String mashing" `Quick test_ppshow  ];
      "test print function type", [
        test_case "Simple" `Quick test_print_ty_fun_simple;
        test_case "Left assoc" `Quick test_print_ty_fun_double_left;
        test_case "Right assoc" `Quick test_print_ty_fun_double_right;
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
      ]
    ]
    
(* Build with `ocamlbuild -pkg alcotest simple.byte` *)
open Libfun
open Syntax

(* The tests *)
let test_string expected ast =
  Alcotest.(check string) expected expected ast

let test_equal expected bool =
    Alcotest.(check bool) (if expected then "equal" else "not equal") expected bool

(*****************************************************************************)
(* ppx_deriving tests *)
let test_ppshow_var () = 
  test_string "(Terms.Var \"toto\")"
    (Terms.(Format.asprintf "%a" pp_term (Var("toto"))))

let test_ppshow_opaque () =
  test_string "(Types.PolymorphicType (<opaque>, (Types.TyBoundVar 0)))"
   (Types.(Format.asprintf "%a" pp_ty (poly_ty "toto" (fun x -> x))))

let test_ppeq_polyty1 () =
  test_equal true (Types.equal_ty
    (poly_ty "titi" (fun x -> x))
    (poly_ty "toto" (fun x -> x)))

let test_ppeq_polyty2 () =
  test_equal true (Types.equal_ty
    (poly_ty "a" (fun x -> poly_ty "b" (fun y -> y => x)))
    (poly_ty "b" (fun x -> poly_ty "a" (fun y -> y => x))))

let test_ppeq_polyty3 () =
  test_equal false (Types.equal_ty
    (poly_ty "a" (fun _x -> poly_ty "b" (fun y -> y => TyFreeVar("c"))))
    (poly_ty "a" (fun _x -> poly_ty "b" (fun y -> y => TyFreeVar("d")))))

let test_ppeq_term_polyty () =
  test_equal true (Terms.equal_term
    (Var "x" ^ (poly_ty "a" (fun x -> poly_ty "b" (fun y -> y => x))))
    (Var "x" ^ (poly_ty "b" (fun x -> poly_ty "a" (fun y -> y => x)))))

(*****************************************************************************)
(* Smart constructeur tests *)
let id = fn "x" (TyFreeVar "X") (fun x -> x)

let poly_id =
  ty_fn "X" (fun _X -> fn "x" _X (fun x -> x))

let test_fn_id () =
  test_string "fun (x: X) = x" (Terms.to_string id)

let test_fn_poly_id () =
  test_string "fun [X] = (fun (x: X) = x)" (Terms.to_string poly_id)

let test_fn2 () =
  test_string "fun (x: X) = (fun (y: X) = (y x))" (Terms.to_string (
    fn "x" (TyFreeVar "X") (fun x -> (fn "y" (TyFreeVar "X") (fun y -> y $ x)))))

let test_letin () =
  test_string "let x = a in (fun (x: X) = x) x" (Terms.to_string (
    letin "x" (Var "a") (fun y -> id $ y)))

let test_poly_ty1 () =
  test_string "forall X. (forall Y. (1 -> 0))" (Types.to_string (
    poly_ty "X" (fun x -> poly_ty "Y" (fun y -> x => y))))

let test_poly_ty2 () =
  test_string "forall X. (a -> (forall Y. (0 * (forall Z. (2 -> 0)))))"
    (Types.to_string (poly_ty "X" (fun x -> TyFreeVar "a" =>
      (poly_ty "Y" (fun y -> tuple [y; poly_ty "Z" (fun z -> x => z)])))))

(*****************************************************************************)
(* Test abstract and fill *)
let test_abstract_fn () =
  test_string "forall a. (0 -> b)"
    (Types.(to_string (abstract "a" (TyFreeVar "a" => TyFreeVar "b"))))

let test_abstract_poly1 () =
  test_string "forall a. (0 -> (forall b. (c * 0 * 1)))"
    (Types.(to_string (abstract "a" (TyFreeVar "a" =>
      PolymorphicType("b", tuple [TyFreeVar "c"; TyBoundVar 0; TyFreeVar "a"])))))

let test_abstract_poly2 () =
  test_string "forall a. (0 -> (forall b. ((forall a. (0 -> 2)) * 0 * 1)))" 
    (Types.(to_string (abstract "a" (TyFreeVar "a" => PolymorphicType("b",
      tuple [PolymorphicType("a", TyBoundVar 0 => TyFreeVar "a"); TyBoundVar 0; TyFreeVar "a"])))))

let test_fill_fn () =
  test_string "T -> b"
  (Types.(to_string (fill (poly_ty "X" (fun x -> x => TyFreeVar "b")) (TyFreeVar "T"))))

let test_fill_poly1 () =
  test_string "c -> (forall Y. (T * 0))"
    (Types.(to_string (fill (poly_ty "X"
      (fun x -> TyFreeVar "c" => poly_ty "Y" (fun y -> tuple [x;y]))) (TyFreeVar "T"))))

let test_fill_poly2 () =
  test_string ""
    Types.(to_string (fill (poly_ty "a" (fun x -> x => (poly_ty "b" (fun y -> tuple [poly_ty "a" (fun y -> y => x); y; x])))) (TyFreeVar "T")))

(*****************************************************************************)
(* Pretty print tests for Types *)
let test_pretty_print expected ast = 
  Alcotest.(check string) expected expected ast

let test_print_ty_fun_simple () =
  let t = (fv "x1") => (fv "x2") in 
  test_pretty_print "x1 -> x2" (Types.to_string t)

let test_print_ty_fun_double_left () =
  let t = (fv "x1" => fv "x2") => (fv "x3") in 
  test_pretty_print "(x1 -> x2) -> x3" (Types.to_string t)

let test_print_ty_fun_double_right () =
  let t = (fv "x1") => (fv "x2" => fv "x3") in
  test_pretty_print "x1 -> (x2 -> x3)" (Types.to_string t)

let test_print_ty_fun_very_long () = (* to test line breaks and indents *)
  let t =  (fv "long_variable_name1" => (fv "long_variable_name2" 
      => (fv "long_variable_name1" => (fv "long_variable_name2"
      => (fv "long_variable_name1" => (fv "long_variable_name2"
      => (fv "long_variable_name1"
      => (fv "long_variable_name 2" => fv "long_variable_name3")))))))) in 
    test_pretty_print "long_variable_name1 ->\n  (long_variable_name2 ->\n    (long_variable_name1 ->\n      (long_variable_name2 ->\n        (long_variable_name1 ->\n          (long_variable_name2 ->\n            (long_variable_name1 ->\n              (long_variable_name 2 -> long_variable_name3)))))))"
        (Types.to_string t)
    
let test_print_poly_type_simple() =
  let t = poly_ty "x1" (fun x -> x) in
  test_pretty_print "forall x1. 0" (Types.to_string t)

let test_print_poly_type_double() =
  let t = poly_ty "x1" (fun x -> poly_ty "x2" (fun _y -> x)) in
  test_pretty_print "forall x1. (forall x2. 1)" (Types.to_string t)

let test_print_poly_type_complex() =
  let t = poly_ty "x1" (fun x -> poly_ty "x2" (fun y -> poly_ty "x3" (fun z -> tuple [z;y;x]))) in
  test_pretty_print "forall x1. (forall x2. (forall x3. (0 * 1 * 2)))" (Types.to_string t)

let test_print_ty_tuple_simple() = 
  let t = tuple [fv "x"; fv "y"; fv "z"]  in
  test_pretty_print "x * y * z" (Types.to_string t)

let test_print_ty_tuple_double() =
  let t = tuple [tuple [fv "x1"; fv "x2"; fv "x3"]; fv "y"; fv "z"] in 
  test_pretty_print "(x1 * x2 * x3) * y * z" (Types.to_string t)

let test_print_ty_compose1() =
  let t = poly_ty "x" (fun x -> x => fv "y") => tuple [fv "a"; fv "b"] in
  test_pretty_print "(forall x. (0 -> y)) -> (a * b)" (Types.to_string t)

let test_print_ty_compose2() =
  let t = tuple [fv "a"; fv "b"] => poly_ty "x" (fun x -> x => fv "y") in
  test_pretty_print "(a * b) -> (forall x. (0 -> y))" (Types.to_string t)

let test_print_ty_compose3() =
  let t = poly_ty "x" (fun x -> tuple [fv "a"; fv "b"] => (x => fv "y")) in 
  test_pretty_print "forall x. ((a * b) -> (0 -> y))" (Types.to_string t)

let test_print_ty_compose4() =
  let t = poly_ty "x" (fun x -> tuple [fv "a"; x => fv "y"] => fv "b") in
  test_pretty_print "forall x. ((a * (0 -> y)) -> b)" (Types.to_string t)

(*****************************************************************************)
(* Pretty print tests for Terms *)
let test_print_variable() = 
  test_pretty_print "toto"
  (Terms.to_string (Var("toto")))

let test_print_fun1() = 
  let t = fn "x" (fv "ty") (fun _ -> Var("y")) in
  test_pretty_print "fun (x: ty) = y" (Terms.to_string t)

let test_print_fun2() = 
  let t = fn "y" (fv "ty1") (fun _ -> (Var("z")) $! (fv "ty2")) in
  test_pretty_print "fun (y: ty1) = z[ty2]"
  (Terms.to_string t)

let test_print_fun3() = 
  let t = fn "x" (poly_ty "poly_ty" (fun _x -> fv "ty1" => fv "ty2"))
    (fun _ -> Var "z") in
  test_pretty_print "fun (x: forall poly_ty. (ty1 -> ty2)) = z"
  (Terms.to_string t)

let test_print_fun4() = 
  let t = fn "x"
    (poly_ty "poly_ty" (fun _x -> fv "ty1" => (fv "ty2" => (fv "ty3" => fv "ty4"))))
    (fun _ -> (fn "y" (fv "ty4" => fv "ty5")
      (fun _ -> fn "z" (fv "ty6" => fv "ty7") (fun _ -> Var("z"))))) in 
  test_pretty_print "fun (x: forall poly_ty. (ty1 -> (ty2 -> (ty3 -> ty4)))) =\n  (fun (y: ty4 -> ty5) = (fun (z: ty6 -> ty7) = z))"
  (Terms.to_string t)

let test_print_fun_apply1() =
  let t = Var "f" $ Var "x" in 
  test_pretty_print "f x" (Terms.to_string t)

let test_print_fun_apply2() =
  let t = (Var "x" $ Var "y") 
          $ (fn "z" (fv "ty") (fun z -> z $ Var "x"))in 
  test_pretty_print "(x y) (fun (z: ty) = (z x))"
  (Terms.to_string t)

let test_print_let1() =
  let t = letin "x" (Var("y")) (fun _ -> Var("z")) in 
  test_pretty_print "let x = y in z" (Terms.to_string t)

let test_print_let2() = 
  let t = letin "x" (Var "f" $ Var "y") (fun _ -> Var "g" $ Var "toto") in 
  test_pretty_print "let x = (f y) in g toto" (Terms.to_string t)

let test_print_let3() = 
  let t = letin "x" (Var "f" $ Var "y")
          (fun _ -> Var "g" $ 
            letin "x" (Var "f2" $ Var "y2")
              (fun _ -> letin "x2" (Var "f3" $ Var "y3")
                (fun _ -> Var "g5" $ Var "toto"))) in
  test_pretty_print "let x = (f y) in\ng let x = (f2 y2) in let x2 = (f3 y3) in g5 toto"
  (Terms.to_string t)         

let test_print_type_abstraction1() = 
  let t = ty_fn "tyvar" (fun _ -> Var "y") in
  test_pretty_print "fun [tyvar] = y" (Terms.to_string t)

let test_print_type_abstraction2() = 
  let t = ty_fn "tyvar" (fun _ -> fn "x" (fv "ty2") (fun _ -> Var "z"))  in 
  test_pretty_print "fun [tyvar] = (fun (x: ty2) = z)"
  (Terms.to_string t)

let test_print_type_apply1() =
  let t = Var "x" $! fv "y" in
  test_pretty_print "x[y]" (Terms.to_string t)

let test_print_type_apply2() = 
  let t =
    (fn "x" (fv "ty") (fun _ -> Var "g" $ letin "x" (Var "f2" $ Var "y2")
      (fun _ -> Var"y")))
    $! poly_ty "x" (fun _ -> tuple [fv "a"; fv "tyvar3" => fv "tyvar4"] => fv "b") in 
  test_pretty_print "(fun (x: ty) = (g let x = (f2 y2) in y))[forall x.\n  ((a * (tyvar3 -> tyvar4)) -> b)]"
  (Terms.to_string t)

(*****************************************************************************)
(* free_vars tests *)
let checkVarSet = Alcotest.(slist string String.compare)
let set_to_list = fun s -> List.of_seq (Terms.VarSet.to_seq s)
let list_to_string l = (* prints a list of string as [x1; x2; ...; xn;]*)
  let s = List.fold_left (fun x acc -> String.concat "" [acc; "; "; x]) "" l in   
  String.concat "" ["["; s; "]"]

let test_free_vars l ast = 
  Alcotest.(check checkVarSet) (list_to_string l) l ast
let test_free_vars_var() =
  test_free_vars ["a"]
  (set_to_list (Terms.free_vars (Var "a")))

let test_free_vars_fun1() =
  let t = fn "b" (fv "ty") (fun _ -> Var "a") in 
  test_free_vars ["a"]
  (set_to_list (Terms.free_vars t))
let test_free_vars_fun2() =
  let t = fn "b" (fv "ty") (fun x -> fn "a" (fv "ty") (fun _ -> x)) in 
  test_free_vars []
  (set_to_list (Terms.free_vars t))

let test_free_vars_funApply1() =
  let t = (fn "a" (fv "ty") (fun _ -> Var "b"))
          $ (fn "b" (fv "ty") (fun _ -> Var "a")) in 
  test_free_vars ["a"; "b"]
  (set_to_list (Terms.free_vars t))

let test_free_vars_funApply2() =
  let t = 
    ((((Var "y" $ Var "v") $ (fn "x" (fv "ty") (fun x -> x))) 
        $ ((Var "z" $ Var "x")$ (Var "y" $ Var "x"))))
    $ ((fn "z" (fv "ty") (fun _ -> Var "x")) 
        $ (fn "u" (fv "ty") (fun x -> x))) 
  in 
  test_free_vars ["z"; "x"; "y"; "v"]
  (set_to_list (Terms.free_vars t))

let test_free_vars_Let1() =
  let t = letin "a" ((fn "b" (fv "ty") (fun _ -> Var "a"))
                      $ (letin "c" (Var "d") (fun x -> x))) 
                (fun _ -> Var "e") in
  test_free_vars ["a";"d";"e"]
  (set_to_list (Terms.free_vars t))

let test_free_vars_Let2() = 
  let t = letin "x" (Var "a")
    (fun x -> 
      letin "y" ((fn "z" (fv "ty") (fun _ -> Var "b" $ x)) $ x) 
        (fun y -> y $ x)) in
  test_free_vars ["a"; "b"]
  (set_to_list (Terms.free_vars t))

let test_free_var_type_abstraction() =
  let t = ty_fn "ty" 
        (fun x -> letin "a" ((fn "b" x (fun _ -> Var "e")) 
                  $ letin "c" (Var "d") (fun y -> y))
                  (fun z -> z))
   in 
  test_free_vars ["d";"e"]
  (set_to_list (Terms.free_vars t))

let test_free_var_type_apply() =
  let t = (letin "a" ((fn "b" (fv "ty") (fun _ -> Var "a")) 
                      $ (letin "c" (Var "d") (fun c -> c)))
                      (fun _ -> Var "e"))
          $! fv "ty" in 
  test_free_vars ["a";"d";"e"]
  (set_to_list (Terms.free_vars t))

let test_free_var_type_annotation() =
  let t = (letin "a" ((fn "b" (fv "ty") (fun _ -> Var "a")) 
                      $ (letin "c" (Var "d") (fun x -> x))) 
            (fun _ -> Var "e")) 
          ^ fv "ty" in 
  test_free_vars ["a";"d";"e"]
  (set_to_list (Terms.free_vars t))

(* Run it *)
let () =
  let open Alcotest in
  run "Utils" [
      "test smart constructors", [
        test_case "fn id" `Quick test_fn_id;
        test_case "fn poly id" `Quick test_fn_poly_id;
        test_case "fn fun 2" `Quick test_fn2;
        test_case "letin" `Quick test_letin;
      ];
      "test abstract", [
        test_case "abstract fn" `Quick test_abstract_fn;
        test_case "abstract poly" `Quick test_abstract_poly1;
        test_case "abstract poly2" `Quick test_abstract_poly2;
      ];
      "test smart constructors poly", [
        test_case "poly type 1" `Quick test_poly_ty1;
        test_case "poly type 2" `Quick test_poly_ty2;
      ];
      "test fill", [
        test_case "fill fn" `Quick test_fill_fn;
        test_case "fill poly" `Quick test_fill_poly1;
        test_case "fill poly2" `Quick test_fill_poly2;
      ];
      "test pp_deriving", [
        test_case "ppshow var" `Quick test_ppshow_var;
        test_case "ppshow opaque polytypes var" `Quick test_ppshow_opaque;
        test_case "ppeq polytype 1b" `Quick test_ppeq_polyty1;
        test_case "ppeq polytype 2b" `Quick test_ppeq_polyty2;
        test_case "ppeq polytype 2b+1f" `Quick test_ppeq_polyty3;
        test_case "ppeq term with" `Quick test_ppeq_term_polyty
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
    
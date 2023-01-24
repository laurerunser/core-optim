open Libfun
open Syntax
open Terms
open Atom

let test_string expected term = Alcotest.(check string) expected expected term

let test_equal_term expected term =
  let check_term = Alcotest.testable pp_term equal_term in
  Alcotest.(check check_term)
    (Format.asprintf "%a" pp_term expected)
    expected term

let x = fresh "x"
let y = fresh "y"
let z = fresh "z"
let a = fresh "a"
let b = fresh "b"
let c = fresh "c"
let tx = fresh "X"
let ty = fresh "Y"
let tz = fresh "Z"
let ts = fresh "S"
let tt = fresh "T"
let f = fresh "f"
let g = fresh "g"
let h = fresh "h"

(*****************************************************************************)
(* pp_term equal_term *)
let test_pp_term () =
  let toto = fresh "toto" in
  test_string
    (Format.asprintf "(Terms.Atom (Terms.Var %a))" pp toto)
    (Format.asprintf "%a" pp_term (Atom (Var toto)))

let test_pp_equal_term () =
  test_equal_term
    (TypeAnnotation
       ( Atom (Var x),
         PolymorphicType
           ("X", PolymorphicType ("Y", TyFun (TyBoundVar 0, TyBoundVar 1))) ))
    (TypeAnnotation
       ( Atom (Var x),
         PolymorphicType
           ("Y", PolymorphicType ("X", TyFun (TyBoundVar 0, TyBoundVar 1))) ))

(*****************************************************************************)
(* smart constructors *)
let id_x = fresh "x"
let id_t = fresh "X"
let id = fn id_x (TyFreeVar id_t) (fun x -> x)
let poly_id_x = fresh "x"
let poly_id_t = fresh "X"
let poly_id = ty_fn poly_id_t (fun t -> fn poly_id_x t (fun x -> x))

let test_fn_id () =
  test_equal_term id (Fun (id_x, TyFreeVar id_t, Atom (Var id_x)))

let test_fn_poly_id () =
  test_equal_term
    (TypeAbstraction
       (poly_id_t, Fun (poly_id_x, TyFreeVar poly_id_t, Atom (Var poly_id_x))))
    poly_id

let test_fn2 () =
  test_equal_term
    (Fun
       ( x,
         TyFreeVar tt,
         Fun (y, TyFreeVar tt, FunApply (Atom (Var y), Atom (Var x))) ))
    (fn x (TyFreeVar tt) (fun x -> fn y (TyFreeVar tt) (fun y -> y $ x)))

let test_fn3 () =
  (* "fun (y: X) = (y (fun (x: X) = (y x)))" *)
  let x1 = fresh "x" in
  let x2 = fresh "x" in
  test_equal_term
    (Fun
       ( x2,
         TyFreeVar tt,
         FunApply
           ( Atom (Var x2),
             Fun (x1, TyFreeVar tt, FunApply (Atom (Var x2), Atom (Var x1))) )
       ))
    (fn x2 (TyFreeVar tt) (fun x -> x $ fn x1 (TyFreeVar tt) (fun z -> x $ z)))

let test_letin () =
  test_equal_term
    (Let (x, Atom (Var a), FunApply (id, Atom (Var x))))
    (letin x (Atom (Var a)) (fun y -> id $ y))

(*****************************************************************************)
(* Pretty print *)
let test_pretty_print expected term =
  Alcotest.(check string) expected expected (to_string term)

let pa = pretty_print_atom

let test_print_variable () =
  let toto = fresh "toto" in
  test_pretty_print (pa toto) (Atom (Var toto))

let test_print_fun1 () =
  let t = fn x (fv tt) (fun _ -> Atom (Var a)) in
  test_pretty_print (Format.sprintf "fun (%s: %s) = %s" (pa x) (pa tt) (pa a)) t

let test_print_fun2 () =
  let t = fn x (fv tt) (fun _ -> Atom (Var a) $! fv ts) in
  test_pretty_print
    (Format.asprintf "fun (%s: %s) = %s[%s]" (pa x) (pa tt) (pa a) (pa ts))
    t

let test_print_fun3 () =
  let t =
    fn x (poly_ty tx (fun _x -> fv ts => fv tt)) (fun _ -> Atom (Var a))
  in
  test_pretty_print
    (Format.sprintf "fun (%s: forall %s. (%s -> %s)) = %s" (pa x) (pa tx)
       (pa ts) (pa tt) (pa a))
    t

let test_print_fun4 () =
  let t1 = fresh "T1" in
  let t2 = fresh "T2" in
  let t3 = fresh "T3" in
  let t4 = fresh "T4" in
  let t5 = fresh "T5" in
  let t6 = fresh "T6" in
  let t7 = fresh "T7" in
  let t =
    fn x
      (poly_ty tx (fun _x -> fv t1 => (fv t2 => (fv t3 => fv t4))))
      (fun _ ->
        fn y
          (fv t4 => fv t5)
          (fun _ -> fn z (fv t6 => fv t7) (fun _ -> Atom (Var z))))
  in
  test_pretty_print
    (Format.sprintf
       "fun (%s:\n\
        forall %s. (%s -> (%s -> (%s -> %s)))) =\n\
       \  (fun (%s: %s -> %s) =\n\
       \    (fun (%s: %s -> %s) = %s))" (pa x) (pa tx) (pa t1) (pa t2) (pa t3)
       (pa t4) (pa y) (pa t4) (pa t5) (pa z) (pa t6) (pa t7) (pa z))
    t

let test_print_fun_apply1 () =
  let t = Atom (Var f) $ Atom (Var x) in
  test_pretty_print (Format.sprintf "%s %s" (pa f) (pa x)) t

let test_print_fun_apply2 () =
  let t =
    Atom (Var a) $ Atom (Var b) $ fn x (fv tt) (fun x -> x $ Atom (Var a))
  in
  test_pretty_print
    (Format.sprintf "(%s %s) (fun (%s: %s) = (%s %s))" (pa a) (pa b) (pa x)
       (pa tt) (pa x) (pa a))
    t

let test_print_let1 () =
  let t = letin x (Atom (Var a)) (fun _ -> Atom (Var b)) in
  test_pretty_print (Format.sprintf "let %s = %s in %s" (pa x) (pa a) (pa b)) t

let test_print_let2 () =
  let t =
    letin x (Atom (Var f) $ Atom (Var a)) (fun _ -> Atom (Var g) $ Atom (Var b))
  in
  test_pretty_print
    (Format.sprintf "let %s = (%s %s) in %s %s" (pa x) (pa f) (pa a) (pa g)
       (pa b))
    t

let test_print_let3 () =
  let f2 = fresh "f2" in
  let f3 = fresh "f3" in
  let f4 = fresh "f4" in
  let toto = fresh "toto" in
  let t =
    letin x (Atom (Var f) $ Atom (Var a)) (fun _ ->
        Atom (Var g)
        $ letin x (Atom (Var f2) $ Atom (Var b)) (fun _ ->
              letin y (Atom (Var f3) $ Atom (Var c)) (fun _ ->
                  Atom (Var f4) $ Atom (Var toto))))
  in
  test_pretty_print
    (Format.sprintf
       "let %s = (%s %s) in\n\
        %s\n\
       \  let %s = (%s %s) in\n\
       \  let %s = (%s %s) in %s %s" (pa x) (pa f) (pa a) (pa g) (pa x) (pa f2)
       (pa b) (pa y) (pa f3) (pa c) (pa f4) (pa toto))
    t

let test_print_type_abstraction1 () =
  let t = ty_fn tt (fun _ -> Atom (Var a)) in
  test_pretty_print (Format.sprintf "fun [%s] = %s" (pa tt) (pa a)) t

let test_print_type_abstraction2 () =
  let t = ty_fn tt (fun _ -> fn x (fv ts) (fun _ -> Atom (Var a))) in
  test_pretty_print
    (Format.sprintf "fun [%s] = (fun (%s: %s) = %s)" (pa tt) (pa x) (pa ts)
       (pa a))
    t

let test_print_type_apply1 () =
  let t = Atom (Var x) $! fv y in
  test_pretty_print (Format.sprintf "%s[%s]" (pa x) (pa y)) t

let test_print_type_apply2 () =
  let t1 = fresh "T1" in
  let t2 = fresh "T2" in
  let t3 = fresh "T3" in
  let t4 = fresh "T4" in
  let t =
    fn x (fv tt) (fun _ ->
        Atom (Var g)
        $ letin x (Atom (Var h) $ Atom (Var b)) (fun _ -> Atom (Var a)))
    $! poly_ty tx (fun _ -> tuple [ fv t1; fv t3 => fv t4 ] => fv t2)
  in
  test_pretty_print
    (Format.sprintf
       "(fun (%s: %s) = (%s let %s = (%s %s) in %s))[forall %s.\n\
       \  ((%s * (%s -> %s)) -> %s)]" (pa x) (pa tt) (pa g) (pa x) (pa h) (pa b)
       (pa a) (pa tx) (pa t1) (pa t3) (pa t4) (pa t2))
    t

(*****************************************************************************)
(* free_vars *)
let checkVarSet =
  Alcotest.(slist (Alcotest.testable Atom.pp Atom.equal) Atom.compare)

let list_to_string l =
  (* prints a list of string as [x1; x2; ...; xn;]*)
  let s =
    List.fold_left
      (fun x acc -> String.concat "" [ acc; "; "; x ])
      ""
      (List.map pretty_print_atom l)
  in
  String.concat "" [ "["; s; "]" ]

let test_free_vars expected term =
  let l = List.of_seq (VarSet.to_seq (free_vars term)) in
  Alcotest.(check checkVarSet) (list_to_string expected) expected l

let test_free_vars_var () =
  let a = fresh "a" in
  test_free_vars [ a ] (Atom (Var a))

let test_free_vars_fun1 () =
  let a = fresh "a" in
  let t = fn (fresh "b") (fv (fresh "ty")) (fun _ -> Atom (Var a)) in
  test_free_vars [ a ] t

let test_free_vars_fun2 () =
  let ty = fresh "ty" in
  let t =
    fn (fresh "b") (fv ty) (fun x -> fn (fresh "a") (fv ty) (fun _ -> x))
  in
  test_free_vars [] t

let test_free_vars_funApply1 () =
  let ty = fresh "ty" in
  let a = fresh "a" in
  let b = fresh "b" in
  let t =
    fn a (fv ty) (fun _ -> Atom (Var b)) $ fn b (fv ty) (fun _ -> Atom (Var a))
  in
  test_free_vars [ a; b ] t

let test_free_vars_funApply2 () =
  let x = fresh "x" in
  let y = fresh "y" in
  let z = fresh "z" in
  let v = fresh "v" in
  let ty = fresh "ty" in
  let t =
    Atom (Var y) $ Atom (Var v)
    $ fn x (fv ty) (fun x -> x)
    $ (Atom (Var z) $ Atom (Var x) $ (Atom (Var y) $ Atom (Var x)))
    $ (fn z (fv ty) (fun _ -> Atom (Var x))
      $ fn (fresh "u") (fv ty) (fun x -> x))
  in
  test_free_vars [ z; x; y; v ] t

let test_free_vars_Let1 () =
  let a = fresh "a" in
  let d = fresh "d" in
  let e = fresh "e" in
  let t =
    letin (fresh "a")
      (fn (fresh "b") (fv (fresh "ty")) (fun _ -> Atom (Var a))
      $ letin (fresh "c") (Atom (Var d)) (fun x -> x))
      (fun _ -> Atom (Var e))
  in
  test_free_vars [ a; d; e ] t

let test_free_vars_Let2 () =
  let a = fresh "a" in
  let b = fresh "b" in
  let t =
    letin (fresh "x") (Atom (Var a)) (fun x ->
        letin (fresh "y")
          (fn (fresh "z") (fv (fresh "ty")) (fun _ -> Atom (Var b) $ x) $ x)
          (fun y -> y $ x))
  in
  test_free_vars [ a; b ] t

let test_free_var_type_abstraction () =
  let d = fresh "d" in
  let e = fresh "e" in
  let t =
    ty_fn (fresh "ty") (fun x ->
        letin (fresh "a")
          (fn (fresh "b") x (fun _ -> Atom (Var e))
          $ letin (fresh "c") (Atom (Var d)) (fun y -> y))
          (fun z -> z))
  in
  test_free_vars [ d; e ] t

let test_free_var_type_apply () =
  let a = fresh "a" in
  let d = fresh "d" in
  let e = fresh "e" in
  let ty = fresh "ty" in
  let t =
    letin (fresh "a")
      (fn (fresh "b") (fv ty) (fun _ -> Atom (Var a))
      $ letin (fresh "c") (Atom (Var d)) (fun c -> c))
      (fun _ -> Atom (Var e))
    $! fv ty
  in
  test_free_vars [ a; d; e ] t

let test_free_var_type_annotation () =
  let a = fresh "a" in
  let d = fresh "d" in
  let e = fresh "e" in
  let ty = fresh "ty" in
  let t =
    letin (fresh "a")
      (fn (fresh "b") (fv ty) (fun _ -> Atom (Var a))
      $ letin (fresh "c") (Atom (Var d)) (fun x -> x))
      (fun _ -> Atom (Var e))
    ^ fv ty
  in
  test_free_vars [ a; d; e ] t
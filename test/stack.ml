open Libfun
open Stack
open Atom
open Terms
open Types
open Syntax

(************************************************************)
(* Pretty Print *)
let test_pretty_print expected stack =
  Alcotest.(check string) expected expected (Stack.to_string stack)

let pa = pretty_print_atom
let toto = fresh "toto"
let x = fresh "x"
let y = fresh "y"
let z = fresh "z"
let tx = fresh "X"
let ty = fresh "Y"
let ts = fresh "S"
let tt = fresh "T"

let test_print_fun_frame () =
  let s = [ HoleFun (Var toto) ] in
  test_pretty_print (Printf.sprintf "_ %s\n" (pa toto)) s

let test_print_ty_frame () =
  let s = [ HoleType (TyFreeVar toto) ] in
  test_pretty_print (Printf.sprintf "_[%s]\n" (pa toto)) s

let test_print_empty_stack () = test_pretty_print "" []
(* should print nothing *)

let test_print_big_stack () =
  let s =
    [
      HoleFun (Var toto);
      HoleType (TyFreeVar x);
      HoleFun (Var y);
      HoleType (TyFreeVar x);
    ]
  in
  test_pretty_print
    (Printf.sprintf "_ %s\n_[%s]\n _ %s\n  _[%s]\n" (pa toto) (pa x) (pa y)
       (pa x))
    s

(*****************************************************************************)
(* plug *)

let test_equal_plug_term expected stack term =
  let check_term = Alcotest.testable pp_term equal_term in
  Alcotest.(check check_term)
    (Format.asprintf "%a" pp_term expected)
    expected (plug stack term)

let test_plug_fun1 () =
  let arg = Var x in
  let stack = [ HoleFun arg ] in
  let f = fn y (TyFreeVar ty) (fun _ -> Base (Var toto)) in
  test_equal_plug_term (f $ arg) stack f

let test_plug_fun2 () =
  let arg1, arg2 = (Var y, Var x) in
  let stack = [ HoleFun arg1; HoleFun arg2 ] in
  let f =
    fn (fresh "z1") (TyFreeVar tx) (fun _ ->
        fn (fresh "z2") (TyFreeVar tx) (fun _ -> Base (Var toto)))
  in
  test_equal_plug_term (f $ arg1 $ arg2) stack f

let test_plug_ty1 () =
  let arg = TyFreeVar tx in
  let stack = [ HoleType arg ] in
  let t = ty_fn ty (fun _ -> Base (Var toto)) in
  test_equal_plug_term (t $! arg) stack t

let test_plug_ty2 () =
  let arg1, arg2 = (TyFreeVar ty, TyFreeVar tx) in
  let stack = [ HoleType arg1; HoleType arg2 ] in
  let t =
    ty_fn (fresh "Z1") (fun _ -> ty_fn (fresh "Z2") (fun _ -> Base (Var toto)))
  in
  test_equal_plug_term (t $! arg1 $! arg2) stack t

let test_plug_mix () =
  let arg1, arg2 = (Var x, TyFreeVar tx) in
  let stack = [ HoleFun arg1; HoleType arg2 ] in
  let f =
    fn y (TyFreeVar ty) (fun _ -> ty_fn (fresh "Z") (fun _ -> Base (Var toto)))
  in
  test_equal_plug_term (f $ arg1 $! arg2) stack f

(*****************************************************************************)
(* Simplification *)
let test_simplification expected term =
  let check_term = Alcotest.testable pp_term equal_term in
  Alcotest.(check check_term)
    (Format.asprintf "Term: %a" pp_term term)
    expected
    (simplify term [] VarMap.empty VarMap.empty)

let test_simplification_atom_var () =
  let t = Base (Var x) in
  test_simplification t t

let test_simplification_atom_bool () =
  let t = Base (Bool true) in
  test_simplification t t

let test_simplification_fun1 () =
  let t = fn (fresh "x2") (TyFreeVar tx) (fun _ -> Base (Bool true)) in
  print_string (Terms.to_string t);
  test_simplification t t

let test_simplification_fun2 () =
  let t =
    fn x (TyFreeVar tx) (fun _ ->
        fn y (TyFreeVar ty) (fun y -> Base y) $ Bool true)
  in
  let expected = fn x (TyFreeVar tx) (fun _ -> Base (Bool true)) in
  test_simplification expected t

let test_simplification_funapply1 () =
  let t = fn x (TyFreeVar tx) (fun x -> Base x) $ Var y in
  let expected = Base (Var y) in
  test_simplification expected t

let test_simplification_funapply2 () =
  let t = fn x (TyFreeVar tx) (fun x -> Base x) $ Bool true in
  let expected = Base (Bool true) in
  test_simplification expected t

let test_simplification_funapply3 () =
  let t =
    fn x (TyFreeVar tx) (fun _ -> fn y (TyFreeVar ts) (fun y -> Base y) $ Var y)
    $ Bool true
  in
  let expected = Base (Var y) in
  test_simplification expected t

let test_simplification_let1 () =
  let t = letin x (Base (Var x)) (fun x -> Base x) in
  test_simplification t t

let test_simplification_let2 () =
  let t =
    letin x (fn y (TyFreeVar ts) (fun y -> Base y) $ Var y) (fun x -> Base x)
  in
  let expected = letin x (Base (Var y)) (fun x -> Base x) in
  test_simplification expected t

let test_simplification_let3 () =
  let t =
    letin x
      (fn y (TyFreeVar ts) (fun y -> Base y) $ Var y)
      (fun _ -> fn z (TyFreeVar tt) (fun x -> Base x) $ Bool true)
  in
  let expected = letin x (Base (Var y)) (fun _ -> Base (Bool true)) in
  test_simplification expected t

let test_simplification_typeabstract () =
  let t =
    ty_fn tx (fun _ ->
        letin x
          (fn y (TyFreeVar ts) (fun y -> Base y) $ Var y)
          (fun _ -> fn z (TyFreeVar tt) (fun x -> Base x) $ Bool true))
  in
  let expected =
    ty_fn tx (fun _ -> letin x (Base (Var y)) (fun _ -> Base (Bool true)))
  in
  test_simplification expected t

let test_simplification_typeapply () =
  (* (fun [X] -> let x = (fun (y:S) -> y) y in fun (z:T) -> z true)
      T
      -> the type abstraction on the first line, applied to T
  *)
  let t =
    ty_fn tx (fun _ ->
        letin x
          (fn y (TyFreeVar ts) (fun y -> Base y) $ Var y)
          (fun _ -> fn z (TyFreeVar tt) (fun x -> Base x) $ Bool true))
    $! TyFreeVar tt
  in
  let expected =
    letin x (Base (Var y)) (fun _ -> Base (Bool true))
    (* expected is : let x = y in true *)
  in
  test_simplification expected t

let test_simplification_annotation () =
  (* let x = (fun (y:S) -> y) y in (fun (z:T) -> z) true
     with type annotation X *)
  let t =
    letin x
      (fn y (TyFreeVar ts) (fun y -> Base y) $ Var y)
      (fun _ -> fn z (TyFreeVar tt) (fun x -> Base x) $ Bool true)
    ^ TyFreeVar tx
  in
  let expected =
    letin x (Base (Var y)) (fun _ -> Base (Bool true))
    (* expected is: let x = y in true
       (type annotation is thrown away) *)
  in
  test_simplification expected t

let test_simplification_ite_true () =
  let t = ite (Base (Bool true)) (Base (Var x)) (Base (Var y)) in
  let expected = Base (Var x) in
  test_simplification expected t

let test_simplification_ite_false () =
  let t = ite (Base (Bool false)) (Base (Var x)) (Base (Var y)) in
  let expected = Base (Var y) in
  test_simplification expected t

let test_simplification_ite1 () =
  let t =
    ite
      (fn x (TyFreeVar tx) (fun x -> Base x) $ Bool true)
      (Base (Var x)) (Base (Var y))
  in
  let expected = ite (Base (Bool true)) (Base (Var x)) (Base (Var y)) in
  test_simplification expected t

let test_simplification_ite2 () =
  let t =
    ite (Base (Var x))
      (fn x (TyFreeVar tx) (fun x -> Base x) $ Bool true)
      (Base (Var y))
  in
  let expected = ite (Base (Var x)) (Base (Bool true)) (Base (Var y)) in
  test_simplification expected t

let test_simplification_ite3 () =
  let t =
    ite (Base (Var x)) (Base (Var y))
      (fn x (TyFreeVar tx) (fun x -> Base x) $ Bool true)
  in
  let expected = ite (Base (Var x)) (Base (Var y)) (Base (Bool true)) in
  test_simplification expected t

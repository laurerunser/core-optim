open Libfun
open Terms
open Types
open Atom
open Syntax
open Stack

let pa = pretty_print_atom
let toto = fresh "toto"
let x = fresh "x"
let y = fresh "y"
let z = fresh "z"
let tx = fresh "X"
let ty = fresh "Y"
let ts = fresh "S"
let tt = fresh "T"

let test_simplification expected term =
  let check_term = Alcotest.testable pp_term alpha_eq in
  Alcotest.(check check_term)
    (Format.asprintf "Term: %a" pp_term term)
    expected (simplify term)

let test_simplification_atom_bool () =
  let t = Base (Bool true) in
  test_simplification t t

let test_simplification_fun () =
  let t = fn x TyBool (fun x -> Base x) in
  test_simplification t t

let test_simplification_fun1 () =
  (* fun x2:Bool -> true
     Expected: no simplification *)
  let x2 = fresh "x2" in
  let t = fn x2 TyBool (fun _ -> Base (Bool true)) in
  test_simplification t t

let test_simplification_fun2 () =
  (* fun x:Bool -> fun y:Bool -> y true
     Expected: fun x:Bool -> true *)
  let t = fn x TyBool (fun _ -> fn y TyBool (fun y -> Base y) $ Bool true) in
  let expected = fn x TyBool (fun _ -> Base (Bool true)) in
  test_simplification expected t

let test_simplification_funapply1 () =
  let t = fn x TyBool (fun x -> Base x) $ Bool true in
  let expected = Base (Bool true) in
  test_simplification expected t

let test_simplification_funapply2 () =
  let t = fn x TyBool (fun x -> Base x) $ Bool true in
  let expected = Base (Bool true) in
  test_simplification expected t

let test_simplification_funapply3 () =
  let t =
    fn x TyBool (fun x -> fn y TyBool (fun y -> Base y) $ x) $ Bool true
  in
  let expected = Base (Bool true) in
  test_simplification expected t

let test_simplification_let1 () =
  let t = letin x (Base (Bool true)) (fun x -> Base x) in
  test_simplification t t

let test_simplification_let2 () =
  let t =
    letin x (fn y TyBool (fun y -> Base y) $ Bool true) (fun x -> Base x)
  in
  let expected = letin x (Base (Bool true)) (fun x -> Base x) in
  test_simplification expected t

let test_simplification_let3 () =
  let t =
    letin x
      (fn y TyBool (fun y -> Base y) $ Bool true)
      (fun _ -> fn z TyBool (fun x -> Base x) $ Bool true)
  in
  let expected = letin x (Base (Bool true)) (fun _ -> Base (Bool true)) in
  test_simplification expected t

let test_simplification_typeabstract () =
  let t =
    ty_fn tx (fun _ ->
        letin x
          (fn y TyBool (fun y -> Base y) $ Bool true)
          (fun _ -> fn z TyBool (fun x -> Base x) $ Bool true))
  in
  let expected =
    ty_fn tx (fun _ -> letin x (Base (Bool true)) (fun _ -> Base (Bool true)))
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
          (fn y TyBool (fun y -> Base y) $ Bool true)
          (fun _ -> fn z TyBool (fun x -> Base x) $ Bool true))
    $! TyBool
  in
  let expected =
    letin x (Base (Bool true)) (fun _ -> Base (Bool true))
    (* expected is : let x = y in true *)
  in
  test_simplification expected t

let test_simplification_annotation () =
  (* let x = (fun (y:S) -> y) y in (fun (z:T) -> z) true
     with type annotation X *)
  let t =
    letin x
      (fn y TyBool (fun y -> Base y) $ Bool true)
      (fun _ -> fn z TyBool (fun x -> Base x) $ Bool true)
    ^ TyBool
  in
  let expected =
    letin x (Base (Bool true)) (fun _ -> Base (Bool true))
    (* expected is: let x = y in true
       (type annotation is thrown away) *)
  in
  test_simplification expected t

let test_simplification_ite_true () =
  let t = ite (Base (Bool true)) (Base (Bool true)) (Base (Bool false)) in
  let expected = Base (Bool true) in
  test_simplification expected t

let test_simplification_ite_false () =
  let t = ite (Base (Bool false)) (Base (Bool false)) (Base (Bool true)) in
  let expected = Base (Bool true) in
  test_simplification expected t

let test_simplification_ite1 () =
  let t =
    ite
      (fn x TyBool (fun x -> Base x) $ Bool true)
      (Base (Bool false)) (Base (Bool true))
  in
  let expected = Base (Bool false) in
  test_simplification expected t

let test_simplification_fun_if () =
  let t =
    fn x TyBool (fun x -> ite (Base x) (Base (Bool true)) (Base (Bool false)))
    $ Bool true
  in
  let expected = Base (Bool true) in
  test_simplification expected t

let test_simplification_var_if () =
  let t =
    fn x TyBool (fun x ->
        ite (Base x)
          (fn y TyBool (fun x -> Base x) $ Bool true)
          (fn y TyBool (fun x -> Base x) $ Bool false))
  in
  let expected =
    fn x TyBool (fun x -> ite (Base x) (Base (Bool true)) (Base (Bool false)))
  in
  test_simplification expected t

let test_simplification_if_in_if () =
  let t =
    ite
      (ite (Base (Bool true)) (Base (Bool true)) (Base (Bool false)))
      (Base (Bool true)) (Base (Bool false))
  in
  let expected = Base (Bool true) in
  test_simplification expected t
